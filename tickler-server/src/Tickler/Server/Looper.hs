{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Tickler.Server.Looper
    ( runLoopers
    ) where

import Import

import qualified Data.Text as T

import Control.Concurrent
import Control.Concurrent.Async
import Control.Monad.Logger
import Data.Pool
import Database.Persist.Sqlite

import Tickler.Server.OptParse.Types

import Tickler.Server.Looper.Emailer
import Tickler.Server.Looper.TriggeredEmailConverter
import Tickler.Server.Looper.TriggeredEmailScheduler
import Tickler.Server.Looper.TriggeredIntrayItemScheduler
import Tickler.Server.Looper.TriggeredIntrayItemSender
import Tickler.Server.Looper.Triggerer
import Tickler.Server.Looper.Types
import Tickler.Server.Looper.VerificationEmailConverter

-- | Blocks
runLoopers :: Pool SqlBackend -> LooperSettings -> IO ()
runLoopers pool LooperSettings {..} =
    liftIO $
    mapConcurrently_
        (liftIO . runStderrLoggingT)
        [ runLooperWithSets pool looperSetTriggererSets runTriggerer
        , runLooperWithSets pool looperSetEmailerSets runEmailer
        , runLooperWithSets
              pool
              looperSetVerificationEmailConverterSets
              runVerificationEmailConverter
        , runLooperWithSets
              pool
              looperSetTriggeredIntrayItemSchedulerSets
              runTriggeredIntrayItemScheduler
        , runLooperWithSets
              pool
              looperSetTriggeredIntrayItemSenderSets
              runTriggeredIntrayItemSender
        , runLooperWithSets
              pool
              looperSetTriggeredEmailSchedulerSets
              runTriggeredEmailScheduler
        , runLooperWithSets
              pool
              looperSetTriggeredEmailConverterSets
              runTriggeredEmailConverter
        ]

runLooperWithSets ::
       Pool SqlBackend -> LooperSetsWith a -> (a -> Looper b) -> LoggingT IO ()
runLooperWithSets pool lsw func =
    case lsw of
        LooperDisabled -> pure ()
        LooperEnabled period sets ->
            runLooper (runLooperContinuously period $ func sets) pool

runLooperContinuously :: MonadIO m => Int -> m b -> m ()
runLooperContinuously period func = go
  where
    go = do
        void func
        liftIO $ threadDelay $ period * 1000 * 1000
        go
