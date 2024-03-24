{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module LanguageServer (serve) where

import Control.Applicative ((<|>))
import qualified Control.Concurrent.STM as STM
import qualified Control.Exception
import Control.Monad (guard, when)
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
import qualified Reporting.Annotation
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
  | Definition {reqId :: Int, filePath :: FilePath, position :: Reporting.Annotation.Position}
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
          <*> pure (Reporting.Annotation.Position (row + 1) (col + 1))

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

    Definition {reqId = reqId, filePath = filePath, position = position} -> do
      sendCreateWorkDoneProgress "go-to-definition-progress"
      sendProgressBegin "go-to-definition-progress" "👀 Finding definition"

      root <- fmap (Maybe.fromMaybe ".") (getRoot filePath state)
      answer <- Ext.Dev.Find.definition root (Watchtower.Editor.PointLocation filePath position)

      sendProgressEnd "go-to-definition-progress"

      case answer of
        Left err -> do
          respondErr reqId err

        Right (Ext.Dev.Find.DefinitionResult filePath (Ann.Region (Ann.Position sr sc) (Ann.Position er ec))) -> do
          respond reqId $
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
              source <- File.readUtf8 top
              (Ext.Dev.Info warnings docs) <- Ext.Dev.info projectRoot top
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


