{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Watchtower.Server (Flags (..), serve) where

import Control.Applicative ((<|>))
import qualified Control.Exception
import Control.Monad (guard, when)
import Control.Monad.Trans (MonadIO (liftIO))
import Data.Aeson ((.:))
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as AesonTypes
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy.Char8 as LB
import qualified Data.Foldable
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
import qualified Watchtower.Questions
import qualified Watchtower.StaticAssets

newtype Flags = Flags
  { _port :: Maybe Int
  }

readHeader :: IO Int
readHeader = do
  line <- B.hGetLine IO.stdin
  if "Content-Length: " `B.isPrefixOf` line
    then return (read $ B.unpack $ B.drop 16 line)
    else readHeader

serve :: Maybe FilePath -> Flags -> IO ()
serve maybeRoot (Flags maybePort) = do
  logWrite $ "Starting server..." ++ show maybeRoot
  liveState <- Watchtower.Live.init

  loop liveState
  where
    loop liveState = do
      contentLen <- readHeader

      logWrite $ "Content-Length: " ++ show contentLen

      body <- B.hGet IO.stdin (contentLen + 2)

      logWrite $ "Body: " ++ B.unpack body

      case Aeson.eitherDecodeStrict body of
        Left err -> do
          logWrite $ "Error: " ++ err
          loop liveState
        Right (Initialize {reqId = idValue}) -> do
          logWrite "Initialize..."
          respond idValue $
            Aeson.object
              [ "capabilities"
                  Aeson..= Aeson.object
                    [ -- "textDocumentSync" Aeson..= Aeson.object ["openClose" Aeson..= True],
                      -- "hoverProvider" Aeson..= True,
                      "definitionProvider" Aeson..= Aeson.object []
                    ],
                "serverInfo"
                  Aeson..= Aeson.object
                    [ "name" Aeson..= ("my-elm-ls" :: String),
                      "version" Aeson..= ("0.0.1" :: String)
                    ]
              ]
          loop liveState
        Right (Shutdown {reqId = idValue}) -> do
          logWrite "Shut down..."
          respond idValue Aeson.Null
          System.Exit.exitSuccess
        Right Exit -> do
          logWrite "Exiting program..."
          System.Exit.exitSuccess
        Right Initialized -> do
          logWrite "Initialized!"
          loop liveState
        Right (Definition {reqId = idValue, params = params}) -> do
          let row = fromIntegral $ line $ position params
          let col = fromIntegral $ character $ position params
          let position = Reporting.Annotation.Position row col
          let urix = uri $ textDocument params
          let pointLocation = Watchtower.Editor.PointLocation (drop 7 urix) position

          x <- Watchtower.Questions.ask liveState (Watchtower.Questions.FindDefinitionPlease pointLocation)

          logWrite $ "Definition: " ++ show x
          loop liveState

respond :: Int -> Aeson.Value -> IO ()
respond idValue value =
  let header = "Content-Length: " ++ show (B.length content) ++ "\r\n\r\n"
      content = LB.toStrict $ Aeson.encode $ Aeson.object ["id" Aeson..= idValue, "result" Aeson..= value]
   in do
        logWrite $ show (B.pack header `B.append` content)
        B.hPutStr IO.stdout (B.pack header `B.append` content)
        IO.hFlush IO.stdout

logWrite :: String -> IO ()
logWrite str = do
  appendFile "/tmp/lsp.log" ("\n" ++ str)

data MessageHeader = MessageHeader
  { messageStart :: Int,
    contentLen :: Int
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

data Position = Position
  { line :: Int,
    character :: Int
  }
  deriving (Show, Generics.Generic)

data TextDocument = TextDocument
  { uri :: String
  }
  deriving (Show, Generics.Generic)

data Params = Params
  { textDocument :: TextDocument,
    position :: Position
  }
  deriving (Show, Generics.Generic)

data Method
  = Initialize {reqId :: Int}
  | Shutdown {reqId :: Int}
  | Definition {reqId :: Int, params :: Params}
  | Exit
  | Initialized
  deriving (Show, Generics.Generic)

instance Aeson.FromJSON Position

instance Aeson.FromJSON Params

instance Aeson.FromJSON TextDocument

-- instance Aeson.FromJSON Params

instance Aeson.FromJSON Method where
  parseJSON = Aeson.withObject "Method" $ \v -> do
    method <- v .: "method" :: AesonTypes.Parser String
    case method of
      "exit" -> pure Exit
      "initialized" -> pure Initialized
      "initialize" -> Initialize <$> v .: "id"
      "shutdown" -> Shutdown <$> v .: "id"
      "textDocument/definition" ->
        Definition
          <$> v .: "id"
          <*> v .: "params"
      _ -> fail "Unknown method"
