{-# LANGUAGE RecordWildCards #-}

module Tickler.Cli.Commands.Sync
    ( sync
    ) where

import Import

import Data.Mergeless

import Tickler.API

import Tickler.Client

import Tickler.Cli.Client
import Tickler.Cli.OptParse
import Tickler.Cli.Session
import Tickler.Cli.Store

sync :: CliM ()
sync = do
    before <- readStoreOrEmpty
    let req = makeSyncRequest before
    mErrOrStore <- withToken $ \t -> runSingleClient $ clientPostSync t req
    after <-
        case mErrOrStore of
            Nothing -> liftIO $ die "No server configured."
            Just errOrStore ->
                case errOrStore of
                    Left err ->
                        liftIO $ die $ unlines ["Sync failed:", show err]
                    Right resp -> do
                        liftIO $ putStr $ showMergeStats req resp
                        pure $ mergeSyncResponse before resp
    writeStore after

showMergeStats ::
       SyncRequest ItemUUID TypedItem
    -> SyncResponse ItemUUID TypedItem
    -> String
showMergeStats SyncRequest {..} SyncResponse {..} =
    unlines
        [ unwords [show $ length syncResponseAddedItems, "added   remotely"]
        , unwords [show $ length syncRequestUndeletedItems, "deleted remotely"]
        , unwords [show $ length syncResponseNewRemoteItems, "added   locally"]
        , unwords
              [ show $ length syncResponseItemsToBeDeletedLocally
              , "deleted locally"
              ]
        ]
