{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Tickler.Server.Looper (runTicklerLoopers) where

import Control.Monad.Catch as Catch
import Control.Monad.Logger
import Control.Monad.Trans.AWS as AWS (Credentials (..))
import Control.Retry
import Data.Pool
import qualified Data.Text as T
import Data.Time
import Database.Persist.Sqlite
import GHC.Clock (getMonotonicTimeNSec)
import Import
import Looper
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

runTicklerLoopers :: Pool SqlBackend -> Settings -> LoggingT IO ()
runTicklerLoopers pool Settings {..} = do
  let looperEnv = LooperEnv {looperEnvPool = pool}
      looperRunner LooperDef {..} = do
        begin <- liftIO getMonotonicTimeNSec
        recovering
          retryPolicyDefault
          ( skipAsyncExceptions
              ++ [ \_ -> Catch.Handler $ \(se :: SomeException) -> do
                     runDb $
                       insert_
                         AdminNotificationEmail
                           { adminNotificationEmailEmail = Nothing,
                             adminNotificationEmailContents =
                               T.pack $
                                 unlines
                                   [ unwords ["The following exception occurred in the", T.unpack looperDefName, "looper:"],
                                     displayException se
                                   ]
                           }
                     pure True
                 ]
          )
          (const looperDefFunc)
        end <- liftIO getMonotonicTimeNSec
        logInfoNS looperDefName $
          T.pack $
            formatTime
              defaultTimeLocale
              "Done, took %Hh%Mm%Ss"
              (realToFrac (fromIntegral (end - begin) / (1_000_000_000 :: Double)) :: NominalDiffTime)

  runLooper looperEnv $
    runLoopersIgnoreOverrun looperRunner $
      concat
        [ [ mkLooperDef
              "Emailer"
              setEmailerSets
              $ let emailerSettings = EmailerSettings AWS.Discover
                 in runEmailer emailerSettings,
            mkLooperDef
              "Triggerer"
              setTriggererSets
              runTriggerer,
            mkLooperDef
              "TriggeredVerificationEmailConverter"
              setVerificationEmailConverterSets
              $ let verificationEmailConverterSettings =
                      VerificationEmailConverterSettings
                        { verificationEmailConverterSetFromAddress = setVerificationFromEmailAddress,
                          verificationEmailConverterSetFromName = "Tickler Verification",
                          verificationEmailConverterSetWebHost = setWebHost
                        }
                 in runVerificationEmailConverter verificationEmailConverterSettings,
            mkLooperDef
              "TriggeredIntrayItemScheduler"
              setTriggeredIntrayItemSchedulerSets
              runTriggeredIntrayItemScheduler,
            mkLooperDef
              "TriggeredIntrayItemSender"
              setTriggeredIntrayItemSenderSets
              runTriggeredIntrayItemSender,
            mkLooperDef
              "TriggeredEmailScheduler"
              setTriggeredEmailSchedulerSets
              runTriggeredEmailScheduler,
            mkLooperDef "TriggeredEmailConverter" setTriggeredEmailConverterSets $
              let triggeredEmailConverterSettings =
                    TriggeredEmailConverterSettings
                      { triggeredEmailConverterSetFromAddress = setTriggererFromEmailAddress,
                        triggeredEmailConverterSetFromName = "Tickler Triggerer",
                        triggeredEmailConverterSetWebHost = setWebHost
                      }
               in runTriggeredEmailConverter triggeredEmailConverterSettings,
            mkLooperDef "AdminNotificationEmailConverter" setTriggeredEmailConverterSets $
              let adminNotificationEmailConverterSettings =
                    AdminNotificationEmailConverterSettings
                      { adminNotificationEmailConverterSetFromAddress = setAdminNotificationFromEmailAddress,
                        adminNotificationEmailConverterSetFromName = "Tickler Admin Notification",
                        adminNotificationEmailConverterSetToAddress = setAdminNotificationToEmailAddress,
                        adminNotificationEmailConverterSetToName = "Tickler Admin",
                        adminNotificationEmailConverterSetWebHost = setWebHost
                      }
               in runAdminNotificationEmailConverter adminNotificationEmailConverterSettings
          ],
          [ mkLooperDef
              "StripeEventsFetcher"
              (monetisationSetStripeEventsFetcher ms)
              (runStripeEventsFetcher (monetisationSetStripeSettings ms))
            | ms <- maybeToList setMonetisationSettings
          ]
        ]
