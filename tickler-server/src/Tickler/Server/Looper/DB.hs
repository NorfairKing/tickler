module Tickler.Server.Looper.DB
  ( runDb
  ) where

import Import

import Database.Persist.Sqlite

import Tickler.Server.Looper.Types

runDb :: SqlPersistT IO b -> Looper b
runDb query = do
  pool <- asks looperEnvPool
  liftIO $ runSqlPool query pool
