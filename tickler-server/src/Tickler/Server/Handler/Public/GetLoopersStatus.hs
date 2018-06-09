{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Tickler.Server.Handler.Public.GetLoopersStatus
    ( serveGetLoopersStatus
    ) where

import Import

import qualified Control.Concurrent.Async as Async
import qualified Data.Text as T

import Tickler.API

import Tickler.Server.Looper
import Tickler.Server.Types

serveGetLoopersStatus :: TicklerHandler LoopersStatus
serveGetLoopersStatus = do
    handle <- asks envLoopersHandle
    liftIO $ mkLoopersStatus handle

mkLoopersStatus :: LoopersHandle -> IO LoopersStatus
mkLoopersStatus LoopersHandle {..} = do
    emailerLooperStatus <- mkLooperStatus emailerLooperHandle
    triggererLooperStatus <- mkLooperStatus triggererLooperHandle
    verificationEmailConverterLooperStatus <-
        mkLooperStatus verificationEmailConverterLooperHandle
    triggeredIntrayItemSchedulerLooperStatus <-
        mkLooperStatus triggeredIntrayItemSchedulerLooperHandle
    triggeredIntrayItemSenderLooperStatus <-
        mkLooperStatus triggeredIntrayItemSenderLooperHandle
    triggeredEmailSchedulerLooperStatus <-
        mkLooperStatus triggeredEmailSchedulerLooperHandle
    triggeredEmailConverterLooperStatus <-
        mkLooperStatus triggeredEmailConverterLooperHandle
    pure LoopersStatus {..}

mkLooperStatus :: LooperHandle -> IO LooperStatus
mkLooperStatus LooperHandleDisabled = pure LooperStatusDisabled
mkLooperStatus (LooperHandleEnabled a) = do
    merr <- Async.poll a
    pure $
        case merr of
            Nothing -> LooperStatusRunning
            Just (Left err) -> LooperStatusErrored $ T.pack $ ppShow err
            Just (Right ()) -> LooperStatusStopped
