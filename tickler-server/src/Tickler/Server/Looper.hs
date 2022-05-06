{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Tickler.Server.Looper
  ( LoopersHandle (..),
    LooperHandle (..),
    startLoopers,
  )
where

import Control.Concurrent
import Control.Exception
import Control.Monad.Catch as Exception
import Control.Monad.Logger
import Control.Monad.Trans.AWS as AWS (Credentials (..))
import Control.Retry
import Data.Pool
import qualified Data.Text as T
import Data.Time
import Database.Persist.Sqlite
import Import
import Tickler.Data
import Tickler.Server.Looper.AdminNotificationEmailConverter
import Tickler.Server.Looper.DB
import Tickler.Server.Looper.Emailer
import Tickler.Server.Looper.StripeEventsFetcher
import Tickler.Server.Looper.TriggeredEmailConverter
import Tickler.Server.Looper.TriggeredEmailScheduler
import Tickler.Server.Looper.TriggeredIntrayItemScheduler
import Tickler.Server.Looper.TriggeredIntrayItemSender
import Tickler.Server.Looper.Triggerer
import Tickler.Server.Looper.Types
import Tickler.Server.Looper.VerificationEmailConverter
import Tickler.Server.OptParse.Types
import UnliftIO

data LoopersHandle = LoopersHandle
  { emailerLooperHandle :: LooperHandle,
    triggererLooperHandle :: LooperHandle,
    verificationEmailConverterLooperHandle :: LooperHandle,
    triggeredIntrayItemSchedulerLooperHandle :: LooperHandle,
    triggeredIntrayItemSenderLooperHandle :: LooperHandle,
    triggeredEmailSchedulerLooperHandle :: LooperHandle,
    triggeredEmailConverterLooperHandle :: LooperHandle,
    adminNotificationEmailConverterLooperHandle :: LooperHandle,
    stripeEventsFetcherLooperHandle :: LooperHandle,
    stripeEventsRetrierLooperHandle :: LooperHandle
  }

startLoopers :: Pool SqlBackend -> Settings -> Maybe MonetisationSettings -> LoggingT IO LoopersHandle
startLoopers pool Settings {..} mms = do
  let LoopersSettings {..} = setLoopersSettings
  let start ::
        String ->
        LooperSetsWith a ->
        (a -> Looper b) ->
        LoggingT IO LooperHandle
      start = startLooperWithSets pool (monetisationSetStripeSettings <$> mms)
  let emailerSettings = EmailerSettings AWS.Discover
  emailerLooperHandle <-
    start
      "Emailer"
      looperSetEmailerSets
      (\() -> runEmailer emailerSettings)
  triggererLooperHandle <-
    start
      "Triggerer"
      looperSetTriggererSets
      (\() -> runTriggerer)

  let verificationEmailConverterSettings =
        VerificationEmailConverterSettings
          { verificationEmailConverterSetFromAddress = setVerificationFromEmailAddress,
            verificationEmailConverterSetFromName = "Tickler Verification",
            verificationEmailConverterSetWebHost = setWebHost
          }

  verificationEmailConverterLooperHandle <-
    start
      "TriggeredVerificationEmailConverter"
      looperSetVerificationEmailConverterSets
      (\() -> runVerificationEmailConverter verificationEmailConverterSettings)
  triggeredIntrayItemSchedulerLooperHandle <-
    start
      "TriggeredIntrayItemScheduler"
      looperSetTriggeredIntrayItemSchedulerSets
      (\() -> runTriggeredIntrayItemScheduler)
  triggeredIntrayItemSenderLooperHandle <-
    start
      "TriggeredIntrayItemSender"
      looperSetTriggeredIntrayItemSenderSets
      (\() -> runTriggeredIntrayItemSender)
  triggeredEmailSchedulerLooperHandle <-
    start
      "TriggeredEmailScheduler"
      looperSetTriggeredEmailSchedulerSets
      (\() -> runTriggeredEmailScheduler)

  let triggeredEmailConverterSettings =
        TriggeredEmailConverterSettings
          { triggeredEmailConverterSetFromAddress = setTriggererFromEmailAddress,
            triggeredEmailConverterSetFromName = "Tickler Triggerer",
            triggeredEmailConverterSetWebHost = setWebHost
          }
  triggeredEmailConverterLooperHandle <-
    start
      "TriggeredEmailConverter"
      looperSetTriggeredEmailConverterSets
      (\() -> runTriggeredEmailConverter triggeredEmailConverterSettings)

  let adminNotificationEmailConverterSettings =
        AdminNotificationEmailConverterSettings
          { adminNotificationEmailConverterSetFromAddress = setAdminNotificationFromEmailAddress,
            adminNotificationEmailConverterSetFromName = "Tickler Admin Notification",
            adminNotificationEmailConverterSetToAddress = setAdminNotificationToEmailAddress,
            adminNotificationEmailConverterSetToName = "Tickler Admin",
            adminNotificationEmailConverterSetWebHost = setWebHost
          }
  adminNotificationEmailConverterLooperHandle <-
    start
      "AdminNotificationEmailConverter"
      looperSetAdminNotificationEmailConverterSets
      (\() -> runAdminNotificationEmailConverter adminNotificationEmailConverterSettings)
  stripeEventsFetcherLooperHandle <-
    maybe
      (pure LooperHandleDisabled)
      ( \ms ->
          start "StripeEventsFetcher" (monetisationSetStripeEventsFetcher ms) $ \() ->
            runStripeEventsFetcher (monetisationSetStripeSettings ms)
      )
      mms
  stripeEventsRetrierLooperHandle <-
    maybe
      (pure LooperHandleDisabled)
      (\ms -> start "StripeEventsRetrier" (monetisationSetStripeEventsRetrier ms) $ \() -> pure ())
      mms
  pure LoopersHandle {..}

startLooperWithSets ::
  Pool SqlBackend ->
  Maybe StripeSettings ->
  String ->
  LooperSetsWith a ->
  (a -> Looper b) ->
  LoggingT IO LooperHandle
startLooperWithSets pool mss name lsw func =
  case lsw of
    LooperDisabled -> pure LooperHandleDisabled
    LooperEnabled lsc@LooperStaticConfig {..} sets ->
      let env = LooperEnv {looperEnvPool = pool, looperEnvStripeSettings = mss}
       in do
            a <-
              async $
                runLooper
                  ( retryLooperWith name looperStaticConfigRetryPolicy $
                      runLooperContinuously name looperStaticConfigPeriod $
                        func sets
                  )
                  env
            pure $ LooperHandleEnabled a lsc

retryLooperWith :: String -> LooperRetryPolicy -> Looper b -> Looper b
retryLooperWith name LooperRetryPolicy {..} looperFunc =
  let policy = constantDelay looperRetryPolicyDelay <> limitRetries looperRetryPolicyAmount
   in recoverWithAdminNotification name policy $ \RetryStatus {..} -> do
        unless (rsIterNumber == 0) $
          logWarnNS (T.pack name <> "Looper") $
            T.unwords
              [ "Retry number",
                T.pack $ show rsIterNumber,
                "after a total delay of",
                T.pack $ show rsCumulativeDelay
              ]
        looperFunc

recoverWithAdminNotification ::
  String -> RetryPolicyM Looper -> (RetryStatus -> Looper a) -> Looper a
recoverWithAdminNotification name set = recovering set handlers
  where
    handlers = skipAsyncExceptions ++ [h]
    h :: RetryStatus -> Exception.Handler Looper Bool
    h _ =
      Exception.Handler $ \(e :: SomeException) -> do
        runDb $
          insert_
            AdminNotificationEmail
              { adminNotificationEmailEmail = Nothing,
                adminNotificationEmailContents =
                  T.pack $
                    unlines
                      [ unwords ["The following exception occurred in the", name, "looper:"],
                        displayException e
                      ]
              }
        return True

runLooperContinuously :: (MonadIO m, MonadLogger m) => String -> Int -> m b -> m ()
runLooperContinuously name period func = go
  where
    go = do
      logInfoNS (T.pack name <> "Looper") "Starting"
      start <- liftIO getCurrentTime
      void func
      end <- liftIO getCurrentTime
      let diff = diffUTCTime end start
      logInfoNS (T.pack name <> "Looper") $ T.unwords ["Finished, took", T.pack $ show diff]
      liftIO $
        threadDelay $
          period * 1000 * 1000
            - fromInteger (diffTimeToPicoseconds (realToFrac diff :: DiffTime) `div` (1000 * 1000))
      go
