{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module LanguageServer (serve) where

import Control.Applicative ((<|>))
import qualified Control.Concurrent.STM as STM
import qualified Control.Exception
import Control.Monad (guard, when, foldM, mapM)
import Control.Monad.Trans (MonadIO (liftIO))
import Data.Aeson ((.:))
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as AesonTypes
import qualified Data.ByteString.Builder
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy.Char8 as LB
import qualified Data.Foldable
import Data.List as List
import Data.Maybe as Maybe
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.Encoding.Error as Aeson
import qualified Data.Name as Name
import qualified Data.Text.Lazy.Builder as TB
import qualified Data.Text.Lazy.IO as TIO
import qualified Develop.Generate.Help
import qualified Ext.Common
import qualified Ext.CompileMode
import qualified Ext.FileCache as FileCache
import qualified Ext.Filewatch
import qualified Ext.Dev.Find
import qualified Ext.Log
import qualified GHC.Generics as Generics
import qualified Json.Encode
import qualified Build
import qualified Reporting.Annotation as Ann
import qualified Snap.Core hiding (path)
import qualified Snap.Http.Server
import Snap.Util.FileServe
import qualified System.Directory as Dir
import qualified System.Exit
import qualified System.IO as IO
import qualified Text.Parsec as Parsec
import qualified Text.Parsec.String as Parsec
import qualified Watchtower.Editor
import qualified Watchtower.Live
import qualified Watchtower.Live.Compile
import qualified Watchtower.Live.Client as Client
import qualified System.FilePath as FilePath
import qualified Ext.Dev.Project
import qualified Ext.Sentry
import Control.Monad as Monad (foldM, guard, mapM_)
import qualified Data.NonEmptyList as NonEmpty
import Ext.Common
import qualified Ext.Dev
import qualified Reporting.Render.Type.Localizer
import qualified Ext.CompileProxy
import qualified Ext.CompileHelpers.Disk
import qualified Reporting.Report
import qualified Reporting.Exit as Exit
import qualified Reporting.Exit.Help as ExitHelp
import qualified Reporting.Error
import qualified Reporting.Render.Code as Code
import qualified Data.NonEmptyList as NE
import qualified Reporting.Report as Report
import qualified Reporting.Doc
import qualified Text.PrettyPrint.ANSI.Leijen as P
import qualified Reporting.Warning
import qualified Ext.FileCache as File
import qualified Reporting.Error.Syntax
import qualified Stuff
import qualified System.FilePath as Path
import qualified Elm.ModuleName as ModuleName
import qualified Ext.Dev.Package
import Data.Name (Name)
import Control.Concurrent
import qualified Ext.Dev.Find.Source
import qualified AST.Source as Src
import qualified AST.Optimized as Opt
import qualified Data.Bifunctor
import qualified Data.Set as Set
import qualified Elm.Details
import qualified Data.Map as Map
import Debug.Trace (traceShow)

serve :: IO ()
serve = do
  state <- State <$> STM.newTVarIO []

  loop state

  where
    loop state = do
      contentLen <- readHeader

      body <- B.hGet IO.stdin (contentLen + 2)

      case Aeson.eitherDecodeStrict body of
        Left err -> do
          loop state

        Right request -> do
          handleRequest state request
          loop state



-- STATE


data State = State
  { projects :: STM.TVar [ProjectCache]
  }


getRoot :: FilePath -> State -> IO (Maybe FilePath)
getRoot path (State mProjects) =
  do
    projects <- STM.readTVarIO mProjects
    let maybeRoot = getRootHelp path projects Nothing
    case maybeRoot of
      Just root -> pure (Just root)
      Nothing -> Dir.withCurrentDirectory (Path.takeDirectory path) Stuff.findRoot


getRootHelp path projects found =
  case projects of
    [] -> found
    (ProjectCache project _) : remain ->
      if Ext.Dev.Project.contains path project
        then case found of
          Nothing ->
            getRootHelp path remain (Just (Ext.Dev.Project._root project))
          Just root ->
            if List.length (Ext.Dev.Project._root project) > List.length root
              then getRootHelp path remain (Just (Ext.Dev.Project._root project))
              else getRootHelp path remain found
        else getRootHelp path remain found


data ProjectCache = ProjectCache
  { project :: Ext.Dev.Project.Project,
    cache :: Cache
  }

matchingProject :: ProjectCache -> ProjectCache -> Bool
matchingProject (ProjectCache one _) (ProjectCache two _) =
  Ext.Dev.Project.equal one two

discoverProjects :: FilePath -> IO [ProjectCache]
discoverProjects root = do
  projects <- Ext.Dev.Project.discover root

  let projectTails = fmap (getProjectShorthand root) projects
  Ext.Log.log Ext.Log.Live (("👁️  found projects\n" ++ root) <> formatList projectTails)
  Monad.foldM initializeProject [] projects


getProjectShorthand :: FilePath -> Ext.Dev.Project.Project -> FilePath
getProjectShorthand root proj =
  case List.stripPrefix root (Ext.Dev.Project.getRoot proj) of
    Nothing -> "."
    Just "" -> "."
    Just str ->
      str

initializeProject :: [ProjectCache] -> Ext.Dev.Project.Project -> IO [ProjectCache]
initializeProject accum project =
  do
    cache <- cacheInit
    pure (ProjectCache project cache : accum)

data Cache =
  Cache
    { prevPublishedDiagnosticsFiles :: MVar [FilePath]
    , publishedDiagnosticsFiles :: MVar [FilePath]
    }

cacheInit :: IO Cache
cacheInit = do
  prevPublishedDiagnosticsFiles  <- newMVar []
  publishedDiagnosticsFiles  <- newMVar []

  pure (Cache prevPublishedDiagnosticsFiles publishedDiagnosticsFiles)

cacheUpdatePrevPublishedDiagnosticsFiles :: Cache -> ([FilePath] -> [FilePath]) -> IO ()
cacheUpdatePrevPublishedDiagnosticsFiles (Cache prevPublishedDiagnosticsFiles _) f =
  modifyMVar_ prevPublishedDiagnosticsFiles (\a -> pure (f a))

cacheGetPrevPublishedDiagnosticsFiles :: Cache -> IO [FilePath]
cacheGetPrevPublishedDiagnosticsFiles (Cache prevPublishedDiagnosticsFiles _) =
  readMVar prevPublishedDiagnosticsFiles

cacheUpdatePublishedDiagnosticsFiles :: Cache -> ([FilePath] -> [FilePath]) -> IO ()
cacheUpdatePublishedDiagnosticsFiles (Cache _ publishedDiagnosticsFiles) f =
  modifyMVar_ publishedDiagnosticsFiles (\a -> pure (f a))

cacheGetPublishedDiagnosticsFiles :: Cache -> IO [FilePath]
cacheGetPublishedDiagnosticsFiles (Cache _ publishedDiagnosticsFiles) =
  readMVar publishedDiagnosticsFiles

-- HEADER


readHeader :: IO Int
readHeader = do
  line <- B.hGetLine IO.stdin
  if "Content-Length: " `B.isPrefixOf` line
    then return (read $ B.unpack $ B.drop 16 line)
    else readHeader



-- REQUEST


data Request
  = Initialize {reqId :: Int, rootPath :: FilePath}
  | Shutdown {reqId :: Int}
  | Definition {reqId :: Int, filePath :: FilePath, position :: Ann.Position}
  | References
    { reqId :: Int
    , filePath :: FilePath
    , position :: Ann.Position
    }
  | Exit
  | Initialized
  | DocumentSymbol {reqId :: Int, filePath :: FilePath}
  | DidSave {filePath :: FilePath}
  | DidOpen {filePath :: FilePath}
  deriving (Show, Generics.Generic)

data Position = Position
  { line :: Int,
    character :: Int
  }
  deriving (Show, Generics.Generic)

instance Aeson.FromJSON Position


instance Aeson.FromJSON Request where
  parseJSON = Aeson.withObject "Method" $ \v -> do
    method <- v .: "method" :: AesonTypes.Parser String
    case method of
      "exit" ->
        pure Exit

      "initialized" ->
        pure Initialized

      "initialize" -> do
        params <- v .: "params"
        Initialize <$> v .: "id" <*> params .: "rootPath"

      "shutdown" ->
        Shutdown <$> v .: "id"

      "textDocument/definition" -> do
        params <- v .: "params"

        textDocument <- params .: "textDocument"
        uri <- textDocument .: "uri"
        let filePath = drop 7 uri

        position <- params .: "position"
        let row = fromIntegral $ line position
        let col = fromIntegral $ character position

        Definition
          <$> v .: "id"
          <*> pure filePath
          <*> pure (Ann.Position (row + 1) (col + 1))

      "textDocument/references" -> do
        params <- v .: "params"

        textDocument <- params .: "textDocument"
        uri <- textDocument .: "uri"
        let filePath = drop 7 uri

        position <- params .: "position"
        let row = fromIntegral $ line position
        let col = fromIntegral $ character position

        References
          <$> v .: "id"
          <*> pure filePath
          <*> pure (Ann.Position (row + 1) (col + 1))
      "textDocument/documentSymbol" -> do
        params <- v .: "params"

        textDocument <- params .: "textDocument"
        uri <- textDocument .: "uri"
        let filePath = drop 7 uri

        DocumentSymbol
          <$> v .: "id"
          <*> pure filePath

      "textDocument/didSave" -> do
        params <- v .: "params"

        textDocument <- params .: "textDocument"
        uri <- textDocument .: "uri"
        let filePath = drop 7 uri

        pure $ DidSave filePath

      "textDocument/didOpen" -> do
        params <- v .: "params"

        textDocument <- params .: "textDocument"
        uri <- textDocument .: "uri"
        let filePath = drop 7 uri

        pure $ DidOpen filePath

      _ -> fail "Unknown method"


handleRequest :: State -> Request -> IO ()
handleRequest state@(State mProjects) request =
  case request of
    Initialize {reqId = idValue, rootPath = rootPath} -> do
      respond idValue $
        Aeson.object
          [ "capabilities" Aeson..= Aeson.object
            [ "definitionProvider" Aeson..= Aeson.object []
            , "documentSymbolProvider" Aeson..= True
            , "textDocumentSync" Aeson..= Aeson.object
                [ "save" Aeson..= True
                , "openClose" Aeson..= True
                ]
            , "referencesProvider" Aeson..= Aeson.object
              [ "workDoneProgress" Aeson..= True
              ]
            ]
          , "serverInfo" Aeson..= Aeson.object
            [ "name" Aeson..= ("my-elm-ls" :: String)
            , "version" Aeson..= ("0.0.1" :: String)
            ]
          ]
      sendCreateWorkDoneProgress "initialization-progress"
      sendProgressBegin "initialization-progress" "Discovering projects"

      discovered <- discoverProjects rootPath
      STM.atomically $ do
        STM.modifyTVar
          mProjects
          ( \projects ->
              List.foldl
                ( \existing new ->
                    if List.any (matchingProject new) existing
                      then existing
                      else new : existing
                )
                projects
                discovered
          )

      sendProgressEnd "initialization-progress"

    Shutdown {reqId = idValue} -> do
      respond idValue Aeson.Null
      System.Exit.exitSuccess

    Exit ->
      System.Exit.exitSuccess

    Initialized ->
      showMessage MessageTypeInfo "Initialized."

    Definition {reqId = reqId, filePath = path , position = position} -> do
      sendCreateWorkDoneProgress "go-to-definition-progress"
      sendProgressBegin "go-to-definition-progress" ("👀 Finding definition: " ++ show position)

      let location = Watchtower.Editor.PointLocation path position
      root <- fmap (Maybe.fromMaybe ".") (getRoot path state)

      pathAndPos <- findDefinition root location

      case pathAndPos >>= (\(path, _, found) -> Ext.Dev.Find.Source.foundRegion found & fmap (\a -> (path, a)) ) of
        Nothing -> do
          sendProgressEnd "go-to-definition-progress"
          respondErr reqId "Definition not found"

        Just (filePath, region@(Ann.Region (Ann.Position sr sc) (Ann.Position er ec))) ->
          do
            sendProgressBegin "go-to-definition-progress" ("👀 Found definition: " ++ show region)
            sendProgressEnd "go-to-definition-progress"
            respond reqId $
              Aeson.object
                [ "uri" Aeson..= ("file://" ++ filePath :: String),
                  "range"
                    Aeson..= Aeson.object
                      [ "start"
                          Aeson..= Aeson.object
                            [ "line" Aeson..= (sr - 1),
                              "character" Aeson..= (sc - 1)
                            ],
                        "end"
                          Aeson..= Aeson.object
                            [ "line" Aeson..= (er - 1),
                              "character" Aeson..= (ec - 1)
                            ]
                      ]
                ]

    References {reqId = reqId, filePath = filePath, position = position } -> do

      sendCreateWorkDoneProgress "references-token"
      sendProgressBegin "references-token" "🔍 Finding references"

      root <- fmap (Maybe.fromMaybe ".") (getRoot filePath state)
      references <- references root (Watchtower.Editor.PointLocation filePath position)

      sendProgressEnd "references-token"

      respond reqId
        (references
          & map
            (\((filePath, (Ann.Region (Ann.Position sr sc) (Ann.Position er ec)))) ->
              Aeson.object
                [ "uri" Aeson..= ("file://" ++ filePath :: String)
                , "range" Aeson..= Aeson.object
                  [ "start" Aeson..= Aeson.object
                    [ "line" Aeson..= (sr - 1)
                    , "character" Aeson..= (sc - 1)
                    ]
                  , "end" Aeson..= Aeson.object
                    [ "line" Aeson..= (er - 1)
                    , "character" Aeson..= (ec - 1)
                    ]
                  ]
                ]
            )
          & Aeson.toJSON
        )
    DocumentSymbol {reqId = reqId, filePath = filePath} -> do
      sendCreateWorkDoneProgress "document-symbols-progress"
      sendProgressBegin "document-symbols-progress" "🔍 Finding symbols"

      root <- fmap (Maybe.fromMaybe ".") (getRoot filePath state)
      result <- Ext.CompileProxy.parse root filePath 


      case result of
        Right srcModule -> do
          let founds = Ext.Dev.Find.Source.symbols srcModule

          respond reqId $ Ext.Dev.Find.Source.encodeFoundAsLspDocumentSymbols founds

        Left _ -> pure ()

      sendProgressEnd "document-symbols-progress"

    DidSave {filePath = filePath} -> do
      sendCreateWorkDoneProgress "compile-progress"
      sendProgressBegin "compile-progress" "Compiling"

      recompile state filePath

      sendProgressEnd "compile-progress"

    DidOpen {filePath = filePath} -> do
      sendCreateWorkDoneProgress "compile-progress"
      sendProgressBegin "compile-progress" "Compiling"

      recompile state filePath

      sendProgressEnd "compile-progress"


findDefinition :: FilePath -> Watchtower.Editor.PointLocation -> IO (Maybe (FilePath, ModuleName.Raw, Ext.Dev.Find.Source.Found))
findDefinition root point@(Watchtower.Editor.PointLocation path _) = do
    details <- Ext.CompileProxy.loadProject root

    let loadLocal path_ = case Elm.Details._outline details of
                            Elm.Details.ValidApp _ -> Ext.CompileProxy.parse root path_
                            Elm.Details.ValidPkg pkgName _ _ -> do
                              loadedFile <- Ext.CompileProxy.loadPkgFileSource pkgName root path_
                              case loadedFile of
                                Left err -> pure $ Left err
                                Right (_, srcModule) -> pure (Right srcModule)

    result <- loadLocal path

    case result of
      Right srcModule -> do
          case Ext.Dev.Find.Source.definitionAtPoint point srcModule of
            Nothing -> pure Nothing

            Just found@(Ext.Dev.Find.Source.FoundExternalOpts imports name) -> do
              -- FIXME: add hack for List? - no type exists in the core module
              Control.Monad.foldM
                  (\acc mod ->
                   case acc of
                       Nothing -> findExternal root mod name
                       found -> pure found
                  )
                  Nothing
                  imports


            Just found@(Ext.Dev.Find.Source.FoundImport (Src.Import (Ann.At _ mod) _ _)) -> do
                  case Ext.Dev.Project.lookupModulePath details mod of
                    Nothing -> do
                      case Ext.Dev.Project.lookupPkgName details mod of
                        Nothing -> pure Nothing
                        Just pkgName -> do
                          maybeCurrentVersion <- Ext.Dev.Package.getCurrentlyUsedOrLatestVersion "." pkgName

                          case maybeCurrentVersion of
                            Nothing -> pure Nothing

                            Just version -> do
                                packageCache <- Stuff.getPackageCache
                                let home = Stuff.package packageCache pkgName version
                                let path = home Path.</> "src" Path.</> ModuleName.toFilePath mod Path.<.>"elm"

                                loadedFile <- Ext.CompileProxy.loadPkgFileSource pkgName home path

                                pure $ case loadedFile of
                                    Left _ -> Nothing
                                    Right (_, modul@(Src.Module maybeName _ _ _ _ _ _ _ _)) ->
                                      case maybeName of
                                        Just name -> 
                                            ( path
                                            , Src.getName modul
                                            , Ext.Dev.Find.Source.FoundModuleName name
                                            )
                                            & Just

                                        Nothing -> Nothing

                    Just path -> do
                      loadedFile <- loadLocal path
                      pure $ case loadedFile of
                        Left _ -> Nothing
                        Right modul@(Src.Module maybeName _ _ _ _ _ _ _ _) ->
                          case maybeName of
                            Just name -> 
                              Just 
                                ( path
                                , Src.getName modul
                                , Ext.Dev.Find.Source.FoundModuleName name
                                )

                            Nothing -> Nothing

            Just found ->
              pure (Just (path, Src.getName srcModule, found))

      Left err  -> do
          source <- File.readUtf8 path
          logMessage MessageTypeError (ExitHelp.toString (Reporting.Report._message (Reporting.Error.Syntax.toReport (Code.toSource source) err)))

          pure Nothing


findExternal :: FilePath -> Src.Import -> Name -> IO (Maybe (FilePath, ModuleName.Raw, Ext.Dev.Find.Source.Found))
findExternal root (Src.Import (Ann.At _ mod) _ _) name = do
    details <- Ext.CompileProxy.loadProject root

    case Ext.Dev.Project.lookupModulePath details mod of
        Nothing -> do
            case Ext.Dev.Project.lookupPkgName details mod of
                Nothing ->
                    pure Nothing

                Just pkgName -> do
                    maybeCurrentVersion <- Ext.Dev.Package.getCurrentlyUsedOrLatestVersion "." pkgName

                    case maybeCurrentVersion of
                        Nothing -> pure Nothing

                        Just version -> do
                            packageCache <- Stuff.getPackageCache
                            let home = Stuff.package packageCache pkgName version
                            let path = home Path.</> "src" Path.</> ModuleName.toFilePath mod Path.<.>"elm"
                            loadedFile <- Ext.CompileProxy.loadPkgFileSource pkgName home path

                            case loadedFile of
                                Left err -> do
                                    source <- File.readUtf8 path
                                    showMessage MessageTypeError (ExitHelp.toString (Reporting.Report._message (Reporting.Error.Syntax.toReport (Code.toSource source) err)))
                                    pure Nothing

                                Right (_, source) -> do
                                    case Ext.Dev.Find.Source.definitionNamed name source of
                                        Just found -> do 
                                          pure $ Just ( path, Src.getName source, found)

                                        Nothing -> do
                                          pure Nothing

        Just path -> do
            loadedFile <- Ext.CompileProxy.parse root path

            case loadedFile of
                Left _ ->
                    pure Nothing

                Right source ->
                    Ext.Dev.Find.Source.definitionNamed name source
                        & fmap (\found -> (path, Src.getName source, found))
                        & pure


references :: FilePath -> Watchtower.Editor.PointLocation -> IO [(FilePath, Ann.Region)]
references root point = do
    definition <- findDefinition root point
    project <- Ext.CompileProxy.loadProject root

    case definition of
        Just (_, mod, Ext.Dev.Find.Source.FoundValue _ (Ann.At region (Src.Value name _ _ _))) ->
            referencesForNamedIThink root project mod region (Ann.toValue name)

        Just (_, mod, Ext.Dev.Find.Source.FoundUnion _ (Ann.At region (Src.Union name _ _))) ->
            referencesForNamedIThink root project mod region (Ann.toValue name)

        Just (_, mod, Ext.Dev.Find.Source.FoundAlias _ (Ann.At region (Src.Alias name _ _))) ->
            referencesForNamedIThink root project mod region (Ann.toValue name)

        Just (_, mod, Ext.Dev.Find.Source.FoundCtor (Ann.At region name)) ->
            referencesForNamedIThink root project mod region name

        Just (_, mod, Ext.Dev.Find.Source.FoundModuleName (Ann.At _ name)) -> do
            let importers = Ext.Dev.Project.importersOf project name

            (mod : Set.toList importers)
              & Control.Monad.foldM (\acc a -> do
                let maybePath = Ext.Dev.Project.lookupModulePath project a

                case maybePath of
                  Nothing -> pure acc
                  Just path -> do
                    loadedFile <- Ext.CompileProxy.parse root path

                    case loadedFile of
                      Left _ -> pure acc
                      Right (Src.Module _ _ _ imports _ _ _ _ _) -> do
                         imports
                           & find (\(Src.Import (Ann.At _ importName) _ _)  -> importName == name)
                           & maybe acc (\(Src.Import (Ann.At region _) _ _)  -> ( path, region) : acc)
                           & pure
              )
              []

        _ ->
          pure []


referencesForNamedIThink :: 
  FilePath 
  -> Elm.Details.Details
  -> ModuleName.Raw 
  -> Ann.Region
  -> Name
  -> IO [(FilePath, Ann.Region)]
referencesForNamedIThink root project mod defRegion defName = do
    let importers = Ext.Dev.Project.importersOf project mod

    (mod : Set.toList importers)
      & Control.Monad.foldM (\acc modName -> do
        case Ext.Dev.Project.lookupModulePath project modName of
            Nothing -> pure acc
            Just path -> do
                result <- Control.Exception.try (Ext.CompileProxy.parse root path) 
                  :: IO (
                       Either Control.Exception.SomeException 
                         (Either Reporting.Error.Syntax.Error Src.Module)
                     )

                pure $ case result of
                  Right (Right srcModule) -> do
                    let found = Ext.Dev.Find.Source.references mod defName srcModule
                    case found of
                      [] -> acc
                      _ -> acc ++ [ (path, region) | region <- found ]
                  _ -> acc
        )
        []



sendCreateWorkDoneProgress :: String -> IO ()
sendCreateWorkDoneProgress token = do
  sendNotification "window/workDoneProgress/create"
    (Aeson.object
      [ "token" Aeson..= token
      ]
    )


sendProgressBegin :: String -> String -> IO ()
sendProgressBegin token title = do
  sendNotification "$/progress"
    (Aeson.object
      [ "token" Aeson..= token
      , "value" Aeson..= Aeson.object
        [ "kind" Aeson..= ("begin" :: String)
        , "title" Aeson..= title
        -- , "message" Aeson..= ("YOLO" :: String)
        ]
      ]
    )


sendProgressEnd :: String -> IO ()
sendProgressEnd token = do
  sendNotification "$/progress"
    (Aeson.object
      [ "token" Aeson..= token
      , "value" Aeson..= Aeson.object
        [ "kind" Aeson..= ("end" :: String)
        ]
      ]
    )


-- COMPILE



{-| This is called frequently.

Generally when a file change has been saved, or the user has changed what their looking at in the editor.

-}
recompile :: State -> FilePath -> IO ()
recompile (State mProjects) changedFile = do
  projects <- STM.readTVarIO mProjects
  let affectedProjects = Maybe.mapMaybe 
                          (\projCache@(ProjectCache proj _) ->
                            if Ext.Dev.Project.contains changedFile proj then Just projCache else Nothing
                          ) 
                          projects

  case affectedProjects of
      [] ->
          Ext.Log.log Ext.Log.Live "No affected projects"
      _ ->
          pure ()

  trackedForkIO $
    track "recompile" $ do

      -- send down status for
      Monad.mapM_ (recompileFile changedFile []) affectedProjects

      -- Get the status of the entire project
      Monad.mapM_ recompileProject affectedProjects

      Monad.mapM_ 
        (\(ProjectCache _ cache) -> do
          prevPublishedDiagnosticsFiles <- cacheGetPrevPublishedDiagnosticsFiles cache
          publishedDiagnosticsFiles <- cacheGetPublishedDiagnosticsFiles cache

          let diff = List.filter (\a -> List.notElem a publishedDiagnosticsFiles)
                      prevPublishedDiagnosticsFiles 

          mapM_ (\a -> publishReportDiagnostic a 1 []) diff

          cacheUpdatePrevPublishedDiagnosticsFiles cache (\_ -> publishedDiagnosticsFiles)
          cacheUpdatePublishedDiagnosticsFiles cache (\_ -> [])

          pure ()
        )
        affectedProjects

recompileProject :: ProjectCache -> IO ()
recompileProject proj@(ProjectCache (Ext.Dev.Project.Project root _ entrypoints _) cache) =
  case entrypoints of
    [] ->
      do
        Ext.Log.log Ext.Log.Live ("Skipping compile, no entrypoint: " <> root)
        pure ()

    topEntry : remainEntry -> do
        recompileFile topEntry remainEntry proj


recompileFile :: FilePath -> [FilePath] -> ProjectCache -> IO ()
recompileFile top remain projCache@(ProjectCache proj@(Ext.Dev.Project.Project root pRoot entrypoints _) cache) =
    do
      let entry = NonEmpty.List top remain

      -- Compile all changed files
      result <- Ext.CompileHelpers.Disk.compileWithoutJsGen root entry

      -- Send compilation status
      case result of
        Right artifacts -> do
          mapM_
            (\path -> do
              source <- File.readUtf8 path
              (Ext.Dev.Info warnings docs) <- Ext.Dev.info root path

              case warnings of
                Nothing -> pure ()
                Just (sourceMod, warns) -> do
                  publishReportDiagnostic path 2 $
                    map
                      (Reporting.Warning.toReport
                        (Reporting.Render.Type.Localizer.fromModule sourceMod)
                        (Code.toSource source)
                      )
                      warns

                  cacheUpdatePublishedDiagnosticsFiles cache (\a -> path : a)
            )
            (top : remain)

        Left exitReactor -> do
          let report = Exit.reactorToReport exitReactor

          case report of
            ExitHelp.CompilerReport filePath e es ->
              mapM_
                (\(Reporting.Error.Module name path _ source err) -> do
                  publishReportDiagnostic path 1 
                    $ NE.toList 
                    $ Reporting.Error.toReports (Code.toSource source) err

                  cacheUpdatePublishedDiagnosticsFiles cache (\a -> path : a)
                )
                (e : es)

            ExitHelp.Report title maybePath message ->
              showMessage MessageTypeError (ExitHelp.toString (ExitHelp.reportToDoc report))

publishReportDiagnostic :: FilePath -> Int -> [Report.Report] -> IO ()
publishReportDiagnostic filePath severity reports =
  sendNotification "textDocument/publishDiagnostics"
    (Aeson.object
      [ "uri" Aeson..= ("file://" ++ filePath :: String)
      , "diagnostics" Aeson..= map
        (\(Report.Report title (Ann.Region (Ann.Position sr sc) (Ann.Position er ec)) _sgstns message) ->
          Aeson.object
            [ "range" Aeson..= Aeson.object
              [ "start" Aeson..= Aeson.object
                [ "line" Aeson..= (sr - 1)
                , "character" Aeson..= (sc - 1)
                ]
              , "end" Aeson..= Aeson.object
                [ "line" Aeson..= (er - 1)
                , "character" Aeson..= (ec - 1)
                ]
              ]
            , "severity" Aeson..= (severity :: Int)
            , "message" Aeson..= (title ++ "\n\n" ++ Reporting.Doc.toString message :: String)
            ]
        )
        reports
      ]
    )

-- RESPONSE


respond :: Int -> Aeson.Value -> IO ()
respond idValue value =
  let
    header = "Content-Length: " ++ show (B.length content) ++ "\r\n\r\n"
    content = LB.toStrict $ Aeson.encode $ Aeson.object
      [ "id" Aeson..= idValue
      , "result" Aeson..= value
      ]
   in do
   B.hPutStr IO.stdout (B.pack header `B.append` content)
   IO.hFlush IO.stdout


sendNotification :: String -> Aeson.Value -> IO ()
sendNotification method value =
  let
    header = "Content-Length: " ++ show (B.length content) ++ "\r\n\r\n"
    content = LB.toStrict $ Aeson.encode $ Aeson.object
      [ "method" Aeson..= method
      , "params" Aeson..= value
      ]
   in do
   B.hPutStr IO.stdout (B.pack header `B.append` content)
   IO.hFlush IO.stdout


respondErr :: Int -> String -> IO ()
respondErr idValue message =
  let
    header = "Content-Length: " ++ show (B.length content) ++ "\r\n\r\n"
    content = LB.toStrict $ Aeson.encode $ Aeson.object
      [ "id" Aeson..= idValue
      , "error" Aeson..= Aeson.object
        [ "code" Aeson..= (-32603 :: Int)
        , "message" Aeson..= (message :: String)
        ]
      ]
   in do
   B.hPutStr IO.stdout (B.pack header `B.append` content)
   IO.hFlush IO.stdout


data MessageHeader = MessageHeader
  { messageStart :: Int
  , contentLen :: Int
  }
  deriving (Show)


messageHeaderParser :: Parsec.Parser MessageHeader
messageHeaderParser = messageHeaderParserHelp 0
  where
    messageHeaderParserHelp i = Parsec.try (parseContentLength i) <|> parseAnyChar i

    parseContentLength i = do
      _ <- Parsec.string "Content-Length: "
      lenStr <- Parsec.many1 Parsec.digit
      let len = read lenStr
      _ <- Parsec.string "\r\n\r\n"
      return $ MessageHeader {contentLen = len, messageStart = i + length (show len) + 20}

    parseAnyChar i = do
      _ <- Parsec.anyChar
      messageHeaderParserHelp (i + 1)

logMessage :: MessageType -> String -> IO ()
logMessage messageType message =
  sendNotification "window/logMessage"
    (Aeson.object
      [ "type" Aeson..= messageTypeToValue messageType
      , "message" Aeson..= message
      ]
    )

showMessage :: MessageType -> String -> IO ()
showMessage messageType message =
  sendNotification "window/showMessage"
    (Aeson.object
      [ "type" Aeson..= messageTypeToValue messageType
      , "message" Aeson..= message
      ]
    )

data MessageType
  = MessageTypeError
  | MessageTypeWarning
  | MessageTypeInfo
  | MessageTypeLog
  | MessageTypeDebug
  deriving (Show)

messageTypeToValue :: MessageType -> Int
messageTypeToValue messageType =
  case messageType of
    MessageTypeError -> 1
    MessageTypeWarning -> 2
    MessageTypeInfo -> 3
    MessageTypeLog -> 4
    MessageTypeDebug -> 5
