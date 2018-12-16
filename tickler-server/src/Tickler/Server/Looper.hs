{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Tickler.Server.Looper
    ( LoopersHandle(..)
    , LooperHandle(..)
    , startLoopers
    ) where

import Import

import Control.Concurrent
import Control.Concurrent.Async
import Control.Monad.Logger
import Control.Retry
import Data.Pool
import qualified Data.Text as T
import Data.Time
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

data LoopersHandle = LoopersHandle
    { emailerLooperHandle :: LooperHandle
    , triggererLooperHandle :: LooperHandle
    , verificationEmailConverterLooperHandle :: LooperHandle
    , triggeredIntrayItemSchedulerLooperHandle :: LooperHandle
    , triggeredIntrayItemSenderLooperHandle :: LooperHandle
    , triggeredEmailSchedulerLooperHandle :: LooperHandle
    , triggeredEmailConverterLooperHandle :: LooperHandle
    }

startLoopers :: Pool SqlBackend -> LooperSettings -> IO LoopersHandle
startLoopers pool LooperSettings {..} = do
    emailerLooperHandle <-
        startLooperWithSets pool looperSetEmailerSets runEmailer
    triggererLooperHandle <-
        startLooperWithSets pool looperSetTriggererSets runTriggerer
    verificationEmailConverterLooperHandle <-
        startLooperWithSets
            pool
            looperSetVerificationEmailConverterSets
            runVerificationEmailConverter
    triggeredIntrayItemSchedulerLooperHandle <-
        startLooperWithSets
            pool
            looperSetTriggeredIntrayItemSchedulerSets
            runTriggeredIntrayItemScheduler
    triggeredIntrayItemSenderLooperHandle <-
        startLooperWithSets
            pool
            looperSetTriggeredIntrayItemSenderSets
            runTriggeredIntrayItemSender
    triggeredEmailSchedulerLooperHandle <-
        startLooperWithSets
            pool
            looperSetTriggeredEmailSchedulerSets
            runTriggeredEmailScheduler
    triggeredEmailConverterLooperHandle <-
        startLooperWithSets
            pool
            looperSetTriggeredEmailConverterSets
            runTriggeredEmailConverter
    pure LoopersHandle {..}

startLooperWithSets ::
       Pool SqlBackend -> LooperSetsWith a -> (a -> Looper b) -> IO LooperHandle
startLooperWithSets pool lsw func =
    case lsw of
        LooperDisabled -> pure LooperHandleDisabled
        LooperEnabled lsc@LooperStaticConfig {..} sets ->
            let env = LooperEnv {looperEnvPool = pool}
             in do a <-
                       async $
                       runLooper
                           (retryLooperWith looperStaticConfigRetryPolicy $
                            runLooperContinuously looperStaticConfigPeriod $
                            func sets)
                           env
                   pure $ LooperHandleEnabled a lsc

retryLooperWith :: LooperRetryPolicy -> Looper b -> Looper b
retryLooperWith LooperRetryPolicy {..} looperFunc =
    let policy =
            constantDelay looperRetryPolicyDelay <>
            limitRetries looperRetryPolicyAmount
     in recoverAll policy $ \RetryStatus {..} -> do
            unless (rsIterNumber == 0) $
                logWarnNS "Looper" $
                T.unwords
                    [ "Retry number"
                    , T.pack $ show rsIterNumber
                    , "after a total delay of"
                    , T.pack $ show rsCumulativeDelay
                    ]
            looperFunc

runLooperContinuously :: MonadIO m => Int -> m b -> m ()
runLooperContinuously period func = go
  where
    go = do
        start <- liftIO getCurrentTime
        void func
        end <- liftIO getCurrentTime
        let diff = diffUTCTime end start
        liftIO $
            threadDelay $
            (period * 1000 * 1000) -
            (fromInteger
                 (diffTimeToPicoseconds (realToFrac diff :: DiffTime) `div`
                  (1000 * 1000)))
        go
