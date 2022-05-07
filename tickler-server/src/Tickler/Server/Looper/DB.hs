module Tickler.Server.Looper.DB
  ( runDb,
  )
where

import Control.Monad.Logger
import Database.Persist.Sqlite
import Import
import Tickler.Server.Looper.Types

runDb :: SqlPersistT (LoggingT IO) b -> Looper b
runDb query = do
  pool <- asks looperEnvPool
  logFunc <- askLoggerIO
  liftIO $ runLoggingT (runSqlPool query pool) logFunc
