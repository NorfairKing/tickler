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
        LooperEnabled period sets ->
            fmap LooperHandleEnabled $
            async $ runLooper (runLooperContinuously period $ func sets) pool

runLooperContinuously :: MonadIO m => Int -> m b -> m ()
runLooperContinuously period func = go
  where
    go = do
        void func
        liftIO $ threadDelay $ period * 1000 * 1000
        go
