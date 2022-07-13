{-# LANGUAGE OverloadedStrings #-}

module Watchtower.Compile.MemoryCached (compileToJson) where


import Ext.Common
import Json.Encode ((==>))
import qualified BackgroundWriter as BW
import qualified Build
import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as B
import qualified Data.ByteString.Lazy as BSL
import qualified Data.NonEmptyList as NE
import qualified Elm.Details as Details
import qualified Ext.Common
import qualified Ext.FileCache
import qualified Ext.MemoryCached.Build
import qualified Ext.MemoryCached.Details
import qualified Generate
import qualified Generate.Html as Html
import qualified Json.Encode as Encode
import qualified Reporting
import qualified Reporting.Exit as Exit
import qualified Reporting.Task as Task
import qualified Stuff
import qualified System.Directory as Dir
import StandaloneInstances


compileToJson :: FilePath -> NE.List FilePath -> IO (Either Encode.Value Encode.Value)
compileToJson root paths =
  do
    let toBS = BSL.toStrict . B.toLazyByteString
    result <- compile root paths

    pure $
      case result of
        Right builder ->
          Right $
            Encode.object
              [ "compiled" ==> Encode.bool True
              ]
        Left exit -> do
          -- @LAMDERA because we do AST injection, sometimes we might get
          -- an error that actually cannot be displayed, i.e, the reactorToReport
          -- function itself throws an exception, mainly due to use of unsafe
          -- functions like Prelude.last and invariants that for some reason haven't
          -- held with our generated code (usually related to subsequent type inference)
          -- We print out a less-processed version here in debug mode to aid with
          -- debugging in these scenarios, as the browser will just get zero bytes
          -- debugPass "serveElm error" (Exit.reactorToReport exit) (pure ())
          -- Help.makePageHtml "Errors" $ Just $
          Left $ Exit.toJson $ Exit.reactorToReport exit


-- compile :: FilePath -> NE.List FilePath -> IO (Either Exit.Reactor B.Builder)
compile :: FilePath -> NE.List FilePath -> IO (Either Exit.Reactor ())
compile root paths =
  Ext.FileCache.handleIfChanged (NE.toList paths) (compile_ root)


compile_ :: FilePath -> [FilePath] -> IO (Either Exit.Reactor ())
compile_ root paths_ = do
  case paths_ of
    [] -> do
      atomicPutStrLn "🙈 compile avoided"
      pure $ Right ()
    x:xs -> do
      let paths = NE.List x xs
      Ext.Common.debug $ "🚀 compiling " ++ show root ++ " -> " ++ show paths
      Dir.withCurrentDirectory root $
        -- @TODO root lock shouldn't be needed unless we're falling through to disk compile
        BW.withScope $ \scope -> Stuff.withRootLock root $ do
          Task.run $ do
            -- Task.io $ debug $ "🌳🌳🌳🌳🌳🌳🌳🌳🌳🌳"
            details <- Task.eio Exit.ReactorBadDetails $ Ext.MemoryCached.Details.load Reporting.silent scope root
            artifacts <- Task.eio Exit.ReactorBadBuild $ Ext.MemoryCached.Build.fromPathsMemoryCached Reporting.silent root details paths

            -- Task.io $ debug $ "🟠🟠🟠🟠🟠🟠🟠"
            -- javascript <- Task.mapError Exit.ReactorBadGenerate $ Generate.dev root details artifacts
            -- let (NE.List name _) = Ext.MemoryCached.Build.getRootNames artifacts
            -- return $ Html.sandwich name javascript
            pure ()
