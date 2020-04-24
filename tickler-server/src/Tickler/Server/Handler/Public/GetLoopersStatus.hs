{-# LANGUAGE RecordWildCards #-}

module Tickler.Server.Handler.Public.GetLoopersStatus
  ( serveGetLoopersStatus
  ) where

import qualified Control.Concurrent.Async as Async
import qualified Data.Text as T
import Import
import Tickler.API
import Tickler.Server.Looper
import Tickler.Server.OptParse.Types
import Tickler.Server.Types

serveGetLoopersStatus :: TicklerHandler LoopersInfo
serveGetLoopersStatus = do
  handle <- asks envLoopersHandle
  liftIO $ mkLoopersInfo handle

mkLoopersInfo :: LoopersHandle -> IO LoopersInfo
mkLoopersInfo LoopersHandle {..} = do
  emailerLooperInfo <- mkLooperInfo emailerLooperHandle
  triggererLooperInfo <- mkLooperInfo triggererLooperHandle
  verificationEmailConverterLooperInfo <- mkLooperInfo verificationEmailConverterLooperHandle
  triggeredIntrayItemSchedulerLooperInfo <- mkLooperInfo triggeredIntrayItemSchedulerLooperHandle
  triggeredIntrayItemSenderLooperInfo <- mkLooperInfo triggeredIntrayItemSenderLooperHandle
  triggeredEmailSchedulerLooperInfo <- mkLooperInfo triggeredEmailSchedulerLooperHandle
  triggeredEmailConverterLooperInfo <- mkLooperInfo triggeredEmailConverterLooperHandle
  adminNotificationEmailConverterLooperInfo <-
    mkLooperInfo adminNotificationEmailConverterLooperHandle
  stripeEventsFetcherLooperInfo <- mkLooperInfo stripeEventsFetcherLooperHandle
  stripeEventsRetrierLooperInfo <- mkLooperInfo stripeEventsRetrierLooperHandle
  pure LoopersInfo {..}

mkLooperInfo :: LooperHandle -> IO LooperInfo
mkLooperInfo LooperHandleDisabled =
  pure
    LooperInfo
      { looperInfoStatus = LooperStatusDisabled
      , looperInfoPeriod = Nothing
      , looperInfoRetryDelay = Nothing
      , looperInfoRetryAmount = Nothing
      }
mkLooperInfo (LooperHandleEnabled a LooperStaticConfig {..}) = do
  merr <- Async.poll a
  let status =
        case merr of
          Nothing -> LooperStatusRunning
          Just (Left err) -> LooperStatusErrored $ T.pack $ ppShow err
          Just (Right ()) -> LooperStatusStopped
  let LooperRetryPolicy {..} = looperStaticConfigRetryPolicy
  pure $
    LooperInfo
      { looperInfoStatus = status
      , looperInfoPeriod = Just looperStaticConfigPeriod
      , looperInfoRetryDelay = Just looperRetryPolicyDelay
      , looperInfoRetryAmount = Just looperRetryPolicyAmount
      }
