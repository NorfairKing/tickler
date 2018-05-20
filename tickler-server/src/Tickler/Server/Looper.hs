{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Tickler.Server.Looper
    ( runLoopers
    ) where

import Import

import Control.Concurrent
import Control.Concurrent.Async
import Control.Monad.Logger
import Data.Pool
import Database.Persist.Sqlite

import Tickler.Server.OptParse.Types

import Tickler.Server.Looper.Emailer
import Tickler.Server.Looper.Triggerer
import Tickler.Server.Looper.Types

-- | Blocks
runLoopers :: LooperSettings -> IO ()
runLoopers LooperSettings {..} =
    runStderrLoggingT $
    withSqlitePoolInfo looperSetConnectionInfo looperSetConnectionCount $ \pool -> do
        liftIO $ mapConcurrently_
            (liftIO . runStderrLoggingT)
            [ runLooperWithSets pool looperSetTriggerSets runTriggerer
            , runLooperWithSets pool looperSetEmailerSets runEmailer
            ]

runLooperWithSets ::
       Pool SqlBackend -> LooperSetsWith a -> (a -> Looper b) -> LoggingT IO ()
runLooperWithSets pool LooperSetsWith {..} func = do
    runLooper (runLooperContinuously looperSetPeriod $ func looperSets) pool

runLooperContinuously :: MonadIO m => Maybe Int -> m b -> m ()
runLooperContinuously looperSetPeriod func = forM_ looperSetPeriod go
  where
    go period = do
        void $ func
        liftIO $ threadDelay $ period * 1000 * 1000
        go period
