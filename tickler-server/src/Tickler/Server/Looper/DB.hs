module Tickler.Server.Looper.DB
  ( runDb,
  )
where

import Database.Persist.Sqlite
import Import
import Tickler.Server.Looper.Types

runDb :: SqlPersistT IO b -> Looper b
runDb query = do
  pool <- asks looperEnvPool
  liftIO $ runSqlPool query pool
