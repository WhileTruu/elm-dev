{-# LANGUAGE OverloadedStrings #-}

module Watchtower.Live.Compile (compileAll, recompile) where

{-|-}

import qualified Data.NonEmptyList as NonEmpty
import qualified Data.List as List
import qualified Data.Set as Set
import qualified Data.ByteString.Lazy
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.ByteString.Builder

import qualified Control.Concurrent.STM as STM

import qualified Reporting.Render.Type.Localizer
import Control.Monad as Monad (foldM, guard)

import qualified Ext.Sentry
import Ext.Common
import qualified Ext.CompileProxy

import qualified Watchtower.Live.Client as Client
import qualified Watchtower.Websocket

import qualified Ext.Dev.Project
import qualified Ext.Dev.Docs
import qualified Ext.Dev

compileMode = Ext.CompileProxy.compileToJson


{-|
Generally called once when the server starts, this will recompile all discovered projects in State

-}
compileAll :: Client.State -> IO ()
compileAll (Client.State mClients mProjects) = do

  Ext.Common.log "🛫" "Recompile everything"
  trackedForkIO $
    track "recompile all projects" $ do
      projects <- STM.readTVarIO mProjects
      Monad.foldM
        (\files proj -> compileProject mClients proj)
        []
        projects

      Ext.Common.log "🛬"  "Recompile everything finished"


compileProject :: STM.TVar [Client.Client] -> Client.ProjectCache -> IO [FilePath]
compileProject mClients proj@(Client.ProjectCache (Ext.Dev.Project.Project projectRoot entrypoints) cache) =
  case entrypoints of
    [] ->
      do
        Ext.Common.log "Skipping compile, no entrypoint" projectRoot
        pure []

    _ ->
        recompileChangedFile mClients entrypoints proj





{-| This is called frequently.

Generally when a file change has been saved, or the user has changed what their looking at in the editor.



-}
recompile :: Client.State -> [String] -> IO ()
recompile (Client.State mClients mProjects) allChangedFiles = do
  let changedElmFiles = List.filter (\filepath -> ".elm" `List.isSuffixOf` filepath ) allChangedFiles
  if (changedElmFiles /= [])
    then do
      debug $ "🛫  recompile starting: " ++ show changedElmFiles
      projects <- STM.readTVarIO mProjects
      trackedForkIO $
        track "recompile" $ do
          Monad.foldM
            (recompileChangedFile mClients)
            changedElmFiles
            projects

          debug $ "🛬  recompile finished: " ++ show changedElmFiles
    else
        pure ()



recompileChangedFile :: STM.TVar [Client.Client] -> [String] -> Client.ProjectCache -> IO [FilePath]
recompileChangedFile mClients changedFiles projCache@(Client.ProjectCache proj@(Ext.Dev.Project.Project projectRoot entrypoints) cache) =
    do
      case changedFiles of
        [] ->
          do
            pure []
        (top : remain) ->
            if List.any (\f -> Ext.Dev.Project.contains f proj) changedFiles then
              do

                  let entry = NonEmpty.List top remain

                  -- Compile all changed files
                  eitherStatusJson <-
                    compileMode
                      projectRoot
                      entry

                  Ext.Sentry.updateCompileResult cache $
                    pure eitherStatusJson


                  -- Recompile the entire project
                  recompileProjectIfSubFile mClients changedFiles projCache

                  (Ext.Dev.Info warnings docs) <- Ext.Dev.info projectRoot top
                  case warnings of
                    Nothing -> pure ()
                    
                    Just (sourceMod, warns) -> do
                      Ext.Common.log ("Sending down " <> show (length warnings) <> " warnings") "!"
                      Client.broadcast mClients
                        (Client.Warnings top (Reporting.Render.Type.Localizer.fromModule sourceMod) warns)
                      pure ()

                  
                  case docs of
                    Nothing -> pure ()

                    Just docs -> do
                      Ext.Common.log "Sending down docs" "!"
                      Client.broadcast mClients
                        (Client.Docs top [ docs ])
                      pure ()

                   -- Send compilation status
                  case eitherStatusJson of
                    Right statusJson ->
                      pure ()

                    Left errJson ->
                      do
                        Ext.Common.log "Changed file failed" "!"
                        Client.broadcast mClients
                          (Client.ElmStatus [ Client.ProjectStatus proj False errJson ])

                        pure ()

                  pure []

            else
                pure []


{-
This function will recompile a project using the projects entrypoint if the changed file is within a projects root dir.

This does mean that if the detected entrypoint doesn't ultimately import the changed file, then you won't get errors

-}
recompileProjectIfSubFile :: STM.TVar [Client.Client] -> [String] -> Client.ProjectCache -> IO [FilePath]
recompileProjectIfSubFile mClients remainingFiles (Client.ProjectCache proj@(Ext.Dev.Project.Project projectRoot entrypoints) cache) =
    do
      let remaining =
            List.filter
              (\file -> not (Ext.Dev.Project.contains file proj))
              remainingFiles

      let maybeEntry =
            case entrypoints of
              [] ->
                let filesWithinProject =
                      List.filter
                        (\file -> Ext.Dev.Project.contains file proj)
                        remainingFiles
                  in case filesWithinProject of
                      [] -> Nothing
                      top : remain ->
                        Just (NonEmpty.List top remain)
              -- Nothing
              top : remainingEntrypoints ->
                Just (NonEmpty.List top remainingEntrypoints)

      case maybeEntry of
        Nothing ->
          do
            debug $ ("☹️ No detected affected project")
            pure remaining
        Just entry ->
          do
            -- Can compileToJson take multiple entrypoints like elm make?
            eitherStatusJson <-
              compileMode
                projectRoot
                entry

            Ext.Sentry.updateCompileResult cache $
              pure eitherStatusJson

            case eitherStatusJson of
              Right statusJson ->
                do
                  Ext.Common.log "Affected project success" "--"
                  Client.broadcast mClients
                    (Client.ElmStatus [ Client.ProjectStatus proj True statusJson ])

              Left errJson ->
                -- send the errors to any client that's listening
                do
                  Ext.Common.log "Affected project failure" "--"
                  Client.broadcast mClients
                    (Client.ElmStatus [ Client.ProjectStatus proj False errJson ])


            pure remaining
