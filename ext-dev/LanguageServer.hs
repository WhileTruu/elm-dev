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
import qualified Data.Bifunctor
import qualified Data.Set as Set
import qualified Elm.Details
import qualified Data.Map as Map
import Debug.Trace (traceShow)

serve :: IO ()
serve = do
  logWrite "Starting server..."
  state <- State <$> STM.newTVarIO []

  loop state

  where
    loop state = do
      contentLen <- readHeader

      logWrite $ "Content-Length: " ++ show contentLen

      body <- B.hGet IO.stdin (contentLen + 2)

      logWrite $ "Body: " ++ B.unpack body

      case Aeson.eitherDecodeStrict body of
        Left err -> do
          logWrite $ "Error: " ++ err
          loop state

        Right request -> do
          handleRequest state request
          loop state



-- STATE


data State = State
  { projects :: STM.TVar [Client.ProjectCache]
  }


getRoot :: FilePath -> State -> IO (Maybe FilePath)
getRoot path (State mProjects) =
  do
    projects <- STM.readTVarIO mProjects
    pure (getRootHelp path projects Nothing)


getRootHelp path projects found =
  case projects of
    [] -> found
    (Client.ProjectCache project _) : remain ->
      if Ext.Dev.Project.contains path project
        then case found of
          Nothing ->
            getRootHelp path remain (Just (Ext.Dev.Project._root project))
          Just root ->
            if List.length (Ext.Dev.Project._root project) > List.length root
              then getRootHelp path remain (Just (Ext.Dev.Project._root project))
              else getRootHelp path remain found
        else getRootHelp path remain found



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

      discovered <- Watchtower.Live.discoverProjects rootPath
      STM.atomically $ do
        STM.modifyTVar
          mProjects
          ( \projects ->
              List.foldl
                ( \existing new ->
                    if List.any (Client.matchingProject new) existing
                      then existing
                      else new : existing
                )
                projects
                discovered
          )

      sendProgressEnd "initialization-progress"

    Shutdown {reqId = idValue} -> do
      logWrite "Shut down..."
      respond idValue Aeson.Null
      System.Exit.exitSuccess

    Exit -> do
      logWrite "Exiting program..."
      System.Exit.exitSuccess

    Initialized -> do
      sendNotification "window/showMessage"
        (Aeson.object
          [ "type" Aeson..= (3 :: Int)
          , "message" Aeson..= ("Initialized." :: String)
          ]
        )

    Definition {reqId = reqId, filePath = path , position = position} -> do
      sendCreateWorkDoneProgress "go-to-definition-progress"
      sendProgressBegin "go-to-definition-progress" ("👀 Finding definition: " ++ show position)

      let location = Watchtower.Editor.PointLocation path position
      root <- fmap (Maybe.fromMaybe ".") (getRoot path state)

      pathAndPos <- findDefinition root location

      case pathAndPos of
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


    DidSave {filePath = filePath} -> do
      sendCreateWorkDoneProgress "compile-progress"
      sendProgressBegin "compile-progress" "Compiling"

      recompile state [filePath]

      sendProgressEnd "compile-progress"

    DidOpen {filePath = filePath} -> do
      sendCreateWorkDoneProgress "compile-progress"
      sendProgressBegin "compile-progress" "Compiling"

      recompile state [filePath]

      sendProgressEnd "compile-progress"


-- TODO: Move Find IO stuff to Find module
findDefinition :: FilePath -> Watchtower.Editor.PointLocation -> IO (Maybe (FilePath, Ann.Region))
findDefinition root point@(Watchtower.Editor.PointLocation path _) = do
    result <- Ext.CompileProxy.parse root path

    case result of
      Right srcModule -> do
          let
              found =
                  Ext.Dev.Find.Source.definitionAtPoint point srcModule

          logWrite $ "Found: " ++ show found

          case found of
            Nothing ->
                pure Nothing

            Just (Ext.Dev.Find.Source.FoundValue _ (Ann.At region _)) ->
                pure (Just (path, region))

            Just (Ext.Dev.Find.Source.FoundUnion _ (Ann.At region _)) ->
                pure (Just (path, region))

            Just (Ext.Dev.Find.Source.FoundAlias _ (Ann.At region _)) ->
                pure (Just (path, region))

            Just (Ext.Dev.Find.Source.FoundTVar (Ann.At region _)) ->
                pure (Just (path, region))

            Just (Ext.Dev.Find.Source.FoundCtor (Ann.At region _)) ->
                pure (Just (path, region))

            Just (Ext.Dev.Find.Source.FoundDef (Src.Define (Ann.At region _) _ _ _)) ->
                pure (Just (path, region))

            Just (Ext.Dev.Find.Source.FoundDef (Src.Destruct (Ann.At region _) _)) ->
                pure (Just (path, region))

            Just (Ext.Dev.Find.Source.FoundPattern (Ann.At region _)) ->
                pure (Just (path, region))

            Just (Ext.Dev.Find.Source.FoundExternalOpts imports name) -> do
                fun <-
                    Control.Monad.foldM
                        (\acc mod ->
                         case acc of
                             Nothing ->
                                 findExternal root mod name

                             found ->
                                 pure found
                        )
                        Nothing
                        imports

                case fun of
                    Just (extPath, Ext.Dev.Find.Source.FoundValue _ (Ann.At region _)) ->
                        pure (Just (extPath, region))

                    Just (extPath, Ext.Dev.Find.Source.FoundUnion _ (Ann.At region _)) ->
                        pure (Just (extPath, region))

                    Just (extPath, Ext.Dev.Find.Source.FoundAlias _ (Ann.At region _)) ->
                        pure (Just (extPath, region))

                    Just (extPath, Ext.Dev.Find.Source.FoundCtor (Ann.At region _)) ->
                        pure (Just (extPath, region))

                    _ ->
                        pure Nothing
            Just (Ext.Dev.Find.Source.FoundImport (Src.Import (Ann.At _ mod) _ _)) -> do
                  details <- Ext.CompileProxy.loadProject root

                  case Ext.Dev.Project.lookupModulePath details mod of
                      Nothing -> do
                          case Ext.Dev.Project.lookupPkgName details mod of
                              Nothing ->
                                  pure Nothing

                              Just pkgName -> do
                                  maybeCurrentVersion <- Ext.Dev.Package.getCurrentlyUsedOrLatestVersion "." pkgName

                                  case maybeCurrentVersion of
                                      Nothing ->
                                          pure Nothing

                                      Just version -> do
                                          packageCache <- Stuff.getPackageCache
                                          let home = Stuff.package packageCache pkgName version
                                          let path = home Path.</> "src" Path.</> ModuleName.toFilePath mod Path.<.>"elm"

                                          pure (Just (path, Ann.one))

                      Just path ->
                          pure (Just (path, Ann.one))

      Left _  ->
          pure Nothing


findExternal :: FilePath -> Src.Import -> Name -> IO (Maybe (FilePath, Ext.Dev.Find.Source.Found))
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
                        Nothing ->
                            pure Nothing

                        Just version -> do
                            packageCache <- Stuff.getPackageCache
                            let home = Stuff.package packageCache pkgName version
                            let path = home Path.</> "src" Path.</> ModuleName.toFilePath mod Path.<.>"elm"
                            loadedFile <- Ext.CompileProxy.loadPkgFileSource pkgName home path

                            case loadedFile of
                                Left err ->
                                    pure Nothing

                                Right (_, source) ->
                                    Ext.Dev.Find.Source.definitionNamed name source
                                        & fmap (\found -> (path, found))
                                        & pure

        Just path -> do
            loadedFile <- Ext.CompileProxy.parse root path

            case loadedFile of
                Left _ ->
                    pure Nothing

                Right source ->
                    Ext.Dev.Find.Source.definitionNamed name source
                        & fmap (\found -> (path, found))
                        & pure


data Found
    = FoundValue ModuleName.Raw (Ann.Located Src.Value)
    | FoundUnion ModuleName.Raw (Ann.Located Src.Union)
    | FoundAlias ModuleName.Raw (Ann.Located Src.Alias)
    | FoundCtor ModuleName.Raw (Ann.Located Name)
    | FoundModule ModuleName.Raw


findDefinition2 :: FilePath -> Watchtower.Editor.PointLocation -> IO (Maybe Found)
findDefinition2 root point@(Watchtower.Editor.PointLocation path _) = do
    result <- Ext.CompileProxy.parse root path

    case result of
      Right srcModule -> do
          let
              found =
                  Ext.Dev.Find.Source.definitionAtPoint point srcModule

          logWrite $ "Found: " ++ show found

          case found of
            Nothing ->
                pure Nothing

            Just (Ext.Dev.Find.Source.FoundValue _ value) ->
                pure (Just (FoundValue (Src.getName srcModule) value))

            Just (Ext.Dev.Find.Source.FoundUnion _ union) ->
                pure (Just (FoundUnion (Src.getName srcModule) union))

            Just (Ext.Dev.Find.Source.FoundAlias _ alias) ->
                pure (Just (FoundAlias (Src.getName srcModule) alias))

            Just (Ext.Dev.Find.Source.FoundTVar _) ->
                pure Nothing

            Just (Ext.Dev.Find.Source.FoundCtor name) ->
                pure (Just (FoundCtor (Src.getName srcModule) name))

            Just (Ext.Dev.Find.Source.FoundDef _) ->
                pure Nothing

            Just (Ext.Dev.Find.Source.FoundPattern _) ->
                pure Nothing

            Just (Ext.Dev.Find.Source.FoundExternalOpts imports name) -> do
                Control.Monad.foldM
                    (\acc mod ->
                     case acc of
                         Nothing ->
                             findExternal2 root mod name

                         found ->
                             pure found
                    )
                    Nothing
                    imports

            Just (Ext.Dev.Find.Source.FoundImport import_) -> do
                pure (Just (FoundModule (Src.getImportName import_)))

      Left _  ->
          pure Nothing


findExternal2 :: FilePath -> Src.Import -> Name -> IO (Maybe Found)
findExternal2 root (Src.Import (Ann.At _ mod) _ _) name = do
    details <- Ext.CompileProxy.loadProject root

    case Ext.Dev.Project.lookupModulePath details mod of
        Nothing -> do
            case Ext.Dev.Project.lookupPkgName details mod of
                Nothing ->
                    pure Nothing

                Just pkgName -> do
                    maybeCurrentVersion <- Ext.Dev.Package.getCurrentlyUsedOrLatestVersion "." pkgName

                    case maybeCurrentVersion of
                        Nothing ->
                            pure Nothing

                        Just version -> do
                            packageCache <- Stuff.getPackageCache
                            let home = Stuff.package packageCache pkgName version
                            let path = home Path.</> "src" Path.</> ModuleName.toFilePath mod Path.<.>"elm"
                            loadedFile <- Ext.CompileProxy.loadPkgFileSource pkgName home path

                            case loadedFile of
                                Left err ->
                                    pure Nothing

                                Right (_, source) ->
                                    (Ext.Dev.Find.Source.definitionNamed name source
                                        >>= toYolo mod
                                    )
                                        & pure

        Just path -> do
            loadedFile <- Ext.CompileProxy.parse root path

            case loadedFile of
                Left _ ->
                    pure Nothing

                Right source ->
                    (Ext.Dev.Find.Source.definitionNamed name source
                        >>= toYolo mod
                    )
                        & pure


    where
        toYolo mod fun =
            case fun of
                Ext.Dev.Find.Source.FoundValue _ value ->
                    Just (FoundValue mod value)

                Ext.Dev.Find.Source.FoundUnion _ union ->
                    Just (FoundUnion mod union)

                Ext.Dev.Find.Source.FoundAlias _ alias ->
                    Just (FoundAlias mod alias)

                Ext.Dev.Find.Source.FoundCtor name ->
                    Just (FoundCtor mod name)

                _ ->
                    Nothing



references :: FilePath -> Watchtower.Editor.PointLocation -> IO [(FilePath, Ann.Region)]
references root point = do
    definition <- findDefinition2 root point

    maybe (pure []) referencesForDef definition

    where
        referencesForDef def = do
            case def of
                FoundValue mod (Ann.At region (Src.Value name _ _ _)) ->
                    referencesForNamedIThink mod region (Ann.toValue name)

                FoundUnion mod (Ann.At region (Src.Union name _ _)) ->
                    referencesForNamedIThink mod region (Ann.toValue name)

                FoundAlias mod (Ann.At region (Src.Alias name _ _)) ->
                    referencesForNamedIThink mod region (Ann.toValue name)

                FoundCtor mod (Ann.At region name) ->
                    referencesForNamedIThink mod region name

                FoundModule mod -> do
                    project <- Ext.CompileProxy.loadProject root

                    let importers = Ext.Dev.Project.importersOf project mod

                    Control.Monad.foldM
                        (\acc modName -> do
                            case Ext.Dev.Project.lookupModulePath project modName of
                                Nothing ->
                                    pure acc

                                Just path -> do
                                    pure (acc ++ [ (path, Ann.one) ])
                        )
                        []
                        (mod : Set.toList importers)

        referencesForNamedIThink mod defRegion defName = do
            project <- Ext.CompileProxy.loadProject root

            let importers = Ext.Dev.Project.importersOf project mod

            Control.Monad.foldM
                (\acc modName -> do
                    case Ext.Dev.Project.lookupModulePath project modName of
                        Nothing ->
                            pure acc
                        Just path -> do
                            Control.Exception.catch
                                (Ext.CompileProxy.parse root path
                                  & fmap
                                      (\result ->
                                          case result of
                                              Left _ ->
                                                  acc

                                              Right srcModule -> do
                                                  let found = Ext.Dev.Find.Source.references mod defName srcModule
                                                  case found of
                                                      [] ->
                                                          acc

                                                      _ ->
                                                          (acc ++ [ (path, region) | region <- found ])
                                      )
                                )
                                handle

                                where
                                    handle :: Control.Exception.SomeException -> IO [(FilePath, Ann.Region)]
                                    handle _ = pure acc
                )
                []
                (mod : Set.toList importers)


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
recompile :: State -> [String] -> IO ()
recompile (State mProjects) allChangedFiles = do
  let changedElmFiles = List.filter (\filepath -> ".elm" `List.isSuffixOf` filepath ) allChangedFiles

  if changedElmFiles /= [] then do

    projects <- STM.readTVarIO mProjects
    let affectedProjects = Maybe.mapMaybe (toAffectedProject changedElmFiles) projects
    case affectedProjects of
        [] ->
            Ext.Log.log Ext.Log.Live "No affected projects"
        _ ->
            pure ()

    trackedForkIO $
      track "recompile" $ do

        -- send down status for
        Monad.mapM_ recompileFile affectedProjects

        -- Get the status of the entire project
        Monad.mapM_ recompileProject affectedProjects

  else
    pure ()


toAffectedProject :: [String] -> Client.ProjectCache -> Maybe (String, [String], Client.ProjectCache)
toAffectedProject changedFiles projCache@(Client.ProjectCache proj@(Ext.Dev.Project.Project projectRoot entrypoints) cache) =
      case changedFiles of
        [] ->
          Nothing

        (top : remain) ->
          if List.any (\f -> Ext.Dev.Project.contains f proj) changedFiles then
            Just (top, remain, projCache)

          else
              Nothing


recompileProject :: (String, [String], Client.ProjectCache) -> IO ()
recompileProject ( _, _, proj@(Client.ProjectCache (Ext.Dev.Project.Project projectRoot entrypoints) cache)) =
  case entrypoints of
    [] ->
      do
        Ext.Log.log Ext.Log.Live ("Skipping compile, no entrypoint: " <> projectRoot)
        pure ()

    topEntry : remainEntry ->
        recompileFile (topEntry, remainEntry, proj)


recompileFile :: (String, [String], Client.ProjectCache) -> IO ()
recompileFile ( top, remain, projCache@(Client.ProjectCache proj@(Ext.Dev.Project.Project projectRoot entrypoints) cache)) =
    do
      let entry = NonEmpty.List top remain

      -- Compile all changed files
      result <- Ext.CompileProxy.compileWithoutJsGen projectRoot entry

      -- TODO: figure out that the following code did :D
      --
      -- Ext.Sentry.updateCompileResult cache $
      --   pure $ case result of
      --     Right _ ->
      --       Right $ Aeson.object [ "compiled" ==> Encode.bool True ]

      --     Left exit -> do
      --       Left $ Exit.toJson $ Exit.reactorToReport exit

      -- Send compilation status
      case result of
        Right _ ->
          mapM_
            (\path -> do
              source <- File.readUtf8 path
              (Ext.Dev.Info warnings docs) <- Ext.Dev.info projectRoot path
              let warningReports = case warnings of
                                    Nothing -> []
                                    Just (sourceMod, warns) ->
                                      map
                                        (Reporting.Warning.toReport
                                          (Reporting.Render.Type.Localizer.fromModule sourceMod)
                                          (Code.toSource source)
                                        )
                                        warns
              -- case docs of
              --   Nothing -> pure ()

              --   Just docs ->
              --     Client.broadcast mClients
              --       (Client.Docs top [ docs ])

              sendNotification "textDocument/publishDiagnostics"
                    (Aeson.object
                      [ "uri" Aeson..= ("file://" ++ path :: String)
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
                            , "severity" Aeson..= (2 :: Int)
                            , "message" Aeson..= (title ++ "\n\n" ++ Reporting.Doc.toString message :: String)
                            ]
                        )
                        warningReports
                      ]
                    )
            )
            (top : remain)

        Left exitReactor -> do
          let report = Exit.reactorToReport exitReactor

          case report of
            ExitHelp.CompilerReport filePath e es ->
              mapM_
                (\(Reporting.Error.Module name path _ source err) ->
                  let
                    reports = Reporting.Error.toReports (Code.toSource source) err

                  in
                  sendNotification "textDocument/publishDiagnostics"
                    (Aeson.object
                      [ "uri" Aeson..= ("file://" ++ path :: String)
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
                            , "severity" Aeson..= (1 :: Int)
                            , "message" Aeson..= (title ++ "\n\n" ++ Reporting.Doc.toString message :: String)
                            ]
                        )
                        (NE.toList reports)
                      ]
                    )
                )
                (e : es)

            ExitHelp.Report title maybePath message ->
              sendNotification "window/showMessage"
                (Aeson.object
                  [ "type" Aeson..= (1 :: Int)
                  , "message" Aeson..= ExitHelp.toString (ExitHelp.reportToDoc report)
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
   logWrite $ show (B.pack header `B.append` content)
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
   logWrite $ show (B.pack header `B.append` content)
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
   logWrite $ show (B.pack header `B.append` content)
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



-- UTILS


logWrite :: String -> IO ()
logWrite str = do
  appendFile "/tmp/lsp.log" (str ++ "\n\n")


