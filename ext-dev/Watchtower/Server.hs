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
import qualified Data.ByteString.Lazy.Char8 as B
import qualified Data.Foldable
import Data.Maybe as Maybe
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
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
import qualified Reporting.Annotation as Ann
import qualified Snap.Core hiding (path)
import qualified Snap.Http.Server
import Snap.Util.FileServe
import qualified System.Directory as Dir
import qualified System.IO as IO
import qualified Text.Parsec as Parsec
import qualified Text.Parsec.String as Parsec
import qualified Watchtower.Live
import qualified Watchtower.Live.Compile
import qualified Watchtower.Questions
import qualified Watchtower.StaticAssets

newtype Flags = Flags
  { _port :: Maybe Int
  }

serve :: Maybe FilePath -> Flags -> IO ()
serve maybeRoot (Flags maybePort) = loop B.empty
  where
    loop buffer = do
      logWrite "Initializing"

      chunk <- B.hGetNonBlocking IO.stdin 4096
      let buffer' = B.append buffer chunk

      when (B.null chunk) $ error "Read no data"

      case Parsec.parse messageHeaderParser "" (B.unpack buffer') of
        Left err -> do
          logWrite $ "Error: " ++ show err
        Right result ->
          if contentLen result > (fromIntegral (B.length buffer') - messageStart result)
            then do
              logWrite "looping now 1"
              loop buffer'
            else do
              logWrite "looping now 2"
              logWrite $ show result
              let input = B.take (fromIntegral (contentLen result)) (B.drop (fromIntegral (messageStart result)) buffer')
              logWrite $ show $ methodFromBytes input

              loop $ B.drop (fromIntegral (contentLen result + messageStart result)) buffer'

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

-- data Params = Params
--   { textDocument :: TextDocument,
--     position :: Position
--   }
--   deriving (Show, Generics.Generic)

data Method
  = Initialize {id :: Int}
  | Shutdown {id :: Int}
  | -- | Definition {id :: Int, params :: Params}
    Exit
  deriving (Show, Generics.Generic)

instance Aeson.FromJSON Position

instance Aeson.FromJSON TextDocument

-- instance Aeson.FromJSON Params

instance Aeson.FromJSON Method where
  parseJSON = Aeson.withObject "Method" $ \v -> do
    method <- v .: "method" :: AesonTypes.Parser String
    case method of
      "initialize" -> Initialize <$> v .: "id"
      "exit" -> pure Exit
      "shutdown" -> Shutdown <$> v .: "id"
      _ -> fail "Unknown method"

-- (Initialize <$> v .: "id")
--   <|> (Shutdown <$> v .: "id")
--   <|> (Definition <$> v .: "id" <*> v .: "params")
--   <|> (pure Exit <* v .: "exit")

methodFromBytes :: B.ByteString -> Either String Method
methodFromBytes = Aeson.eitherDecode
