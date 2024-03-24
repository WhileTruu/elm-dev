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

      _ -> fail "Unknown method"


handleRequest :: State -> Request -> IO ()
handleRequest state@(State mProjects) request =
  case request of
    Initialize {reqId = idValue, rootPath = rootPath} -> do
      logWrite $ "Initialize..." ++ rootPath

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

      respond idValue $
        Aeson.object
          [ "capabilities" Aeson..= Aeson.object
            [ "definitionProvider" Aeson..= Aeson.object []
            , "textDocumentSync" Aeson..= Aeson.object 
                [ "save" Aeson..= True
                -- , "openClose" Aeson..= True
                ]
            ]
          , "serverInfo" Aeson..= Aeson.object
            [ "name" Aeson..= ("my-elm-ls" :: String)
            , "version" Aeson..= ("0.0.1" :: String)
            ]
          ]

    Shutdown {reqId = idValue} -> do
      logWrite "Shut down..."
      respond idValue Aeson.Null
      System.Exit.exitSuccess

    Exit -> do
      logWrite "Exiting program..."
      System.Exit.exitSuccess

    Initialized -> do
      logWrite "Initialized!"

    Definition {reqId = reqId, filePath = filePath, position = position} -> do
      root <- fmap (Maybe.fromMaybe ".") (getRoot filePath state)
      answer <- Ext.Dev.Find.definition root (Watchtower.Editor.PointLocation filePath position)

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
      logWrite ("👀 file saved: " <> FilePath.takeFileName filePath)
      recompile state [filePath]



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

        -- send down warnings and docs
        Monad.mapM_ sendInfo affectedProjects

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
      -- FIXME: remove and replace with sending diagnostics via stdout
      mClients <- STM.newTVarIO []

      let entry = NonEmpty.List top remain

      -- Compile all changed files
      eitherStatusJson <- Ext.CompileProxy.compileToJson projectRoot entry


      Ext.Sentry.updateCompileResult cache $
        pure eitherStatusJson

      -- Send compilation status
      case eitherStatusJson of
        Right statusJson -> do
          Client.broadcast mClients
            (Client.ElmStatus [ Client.ProjectStatus proj True statusJson ])

        Left errJson -> do
          Client.broadcast mClients
            (Client.ElmStatus [ Client.ProjectStatus proj False errJson ])


sendInfo :: (String, [String], Client.ProjectCache) -> IO ()
sendInfo ( top, remain , projCache@(Client.ProjectCache proj@(Ext.Dev.Project.Project projectRoot entrypoints) cache)) = do
    -- FIXME: remove and replace with sending diagnostics via stdout
    mClients <- STM.newTVarIO []

    (Ext.Dev.Info warnings docs) <- Ext.Dev.info projectRoot top

    case warnings of
      Nothing -> pure ()
      
      Just (sourceMod, warns) ->
        Client.broadcast mClients
          (Client.Warnings top (Reporting.Render.Type.Localizer.fromModule sourceMod) warns)

    -- case docs of
    --   Nothing -> pure ()

    --   Just docs ->
    --     Client.broadcast mClients
    --       (Client.Docs top [ docs ])


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
  appendFile "/tmp/lsp.log" ("\n" ++ str)


