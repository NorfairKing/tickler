{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Tickler.Server.Looper
    ( runLoopers
    ) where

import Import

import Control.Concurrent

import Control.Monad.Logger
import Database.Persist.Sqlite

import Tickler.Server.OptParse.Types

import Tickler.Server.Looper.Triggerer
import Tickler.Server.Looper.Types

-- | Blocks
runLoopers :: LooperSettings -> IO ()
runLoopers LooperSettings {..} = runTriggererContinuously looperSetTriggerSets

runTriggererContinuously :: LooperSetsWith TriggerSettings -> IO ()
runTriggererContinuously LooperSetsWith {..} = do
    let TriggerSettings {..} = looperSets
    runStderrLoggingT $
        withSqlitePoolInfo triggerSetConnectionInfo triggerSetConnectionCount $
        runLooper (runLooperContinuously looperSetPeriod runTriggerer)

runLooperContinuously :: MonadIO m => Maybe Int -> m b -> m ()
runLooperContinuously looperSetPeriod func = forM_ looperSetPeriod go
  where
    go period = do
        void $ func
        liftIO $ threadDelay $ period * 1000 * 1000
        go period
