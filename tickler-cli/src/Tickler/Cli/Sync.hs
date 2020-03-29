module Tickler.Cli.Sync
  ( withStoreAndSync
  , modifyStoreAndSync
  , syncAndGet
  , syncAndReturn
  ) where

import Import

import Tickler.Client

import Tickler.Cli.Client
import Tickler.Cli.OptParse
import Tickler.Cli.Session
import Tickler.Cli.Store

withStoreAndSync :: (Store -> CliM Store) -> CliM ()
withStoreAndSync func = do
  before <- readStoreOrEmpty
  processed <- func before
  let req = makeSyncRequest processed
  strat <- asks setSyncStrategy
  mErrOrStore <-
    case strat of
      NeverSync -> pure Nothing
      AlwaysSync -> withToken $ \t -> runSingleClient $ clientPostSync t req
  after <-
    case mErrOrStore of
      Nothing -> pure processed
      Just errOrStore ->
        case errOrStore of
          Left err -> do
            liftIO $
              putStrLn $ unlines ["Sync failed, but store still modified succesfully:", show err]
            pure processed
          Right r -> do
            let after = mergeSyncResponse processed r
            anyUnsyncedWarning after
            pure after
  writeStore after

modifyStoreAndSync :: (Store -> Store) -> CliM ()
modifyStoreAndSync func = withStoreAndSync (pure . func)

syncAndGet :: (Store -> CliM a) -> CliM a
syncAndGet func = do
  before <- readStoreOrEmpty
  let req = makeSyncRequest before
  strat <- asks setSyncStrategy
  mErrOrStore <-
    case strat of
      NeverSync -> pure Nothing
      AlwaysSync -> withToken $ \t -> runSingleClient $ clientPostSync t req
  case mErrOrStore of
    Nothing -> func before
    Just errOrStore ->
      case errOrStore of
        Left err -> do
          liftIO $ putStrLn $ unlines ["Sync failed, but still fetched succesfully:", show err]
          func before
        Right r -> do
          let after = mergeSyncResponse before r
          anyUnsyncedWarning after
          writeStore after
          func after

anyUnsyncedWarning :: Store -> CliM ()
anyUnsyncedWarning after =
  when (anyUnsynced after) $
  liftIO $
  putStrLn $
  unlines
    [ "Not all added items were synchronized in the most recent synchronisation."
    , "This may have occurred if you have not subscribed with your sync server."
    , "If that is the case, please navigate to your sync server's web interface to subscribe."
    ]

syncAndReturn :: (Store -> a) -> CliM a
syncAndReturn func = syncAndGet $ pure . func
