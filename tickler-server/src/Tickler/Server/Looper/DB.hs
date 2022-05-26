module Tickler.Server.Looper.DB
  ( runDB,
  )
where

import Control.Monad.Logger
import Database.Persist.Sqlite
import Import
import Tickler.Server.Looper.Types
import UnliftIO.Resource

runDB :: SqlPersistT (LoggingT (ResourceT IO)) b -> Looper b
runDB query = do
  pool <- asks looperEnvPool
  logFunc <- askLoggerIO
  liftIO $ runResourceT $ runLoggingT (runSqlPool query pool) logFunc
