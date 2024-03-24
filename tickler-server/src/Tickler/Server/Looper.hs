{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Tickler.Server.Looper (runTicklerLoopers) where

import Control.Monad.Catch as Catch
import Control.Monad.Logger
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
      looperRunner LooperDef {..} =
        addLooperNameToLog looperDefName
          $ runLooper looperEnv
          $ do
            logInfoN "Starting"
            begin <- liftIO getMonotonicTimeNSec
            recovering
              retryPolicyDefault
              ( skipAsyncExceptions
                  ++ [ \_ -> Catch.Handler $ \(se :: SomeException) -> do
                         runDB
                           $ insert_
                             AdminNotificationEmail
                               { adminNotificationEmailEmail = Nothing,
                                 adminNotificationEmailContents =
                                   T.pack
                                     $ unlines
                                       [ unwords ["The following exception occurred in the", T.unpack looperDefName, "looper:"],
                                         displayException se
                                       ]
                               }
                         pure True
                     ]
              )
              (const looperDefFunc)
            end <- liftIO getMonotonicTimeNSec
            logInfoN
              $ T.pack
              $ formatTime
                defaultTimeLocale
                "Done, took %Hh%Mm%Ss"
                (realToFrac (fromIntegral (end - begin) / (1_000_000_000 :: Double)) :: NominalDiffTime)

  runLoopersIgnoreOverrun looperRunner
    $ concat
      [ [ mkLooperDef "Emailer" setEmailerSets runEmailer,
          mkLooperDef
            "Triggerer"
            setTriggererSets
            runTriggerer,
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
            runTriggeredEmailScheduler
        ],
        [ mkLooperDef "TriggeredVerificationEmailConverter" setVerificationEmailConverterSets
            $ runVerificationEmailConverter verificationEmailConverterSettings
          | verificationEmailConverterSettings <- maybeToList $ do
              from <- setVerificationFromEmailAddress
              webHost <- setWebHost
              pure
                VerificationEmailConverterSettings
                  { verificationEmailConverterSetFromAddress = from,
                    verificationEmailConverterSetFromName = "Tickler Verification",
                    verificationEmailConverterSetWebHost = webHost
                  }
        ],
        [ mkLooperDef "TriggeredEmailConverter" setTriggeredEmailConverterSets
            $ runTriggeredEmailConverter triggeredEmailConverterSettings
          | triggeredEmailConverterSettings <- maybeToList $ do
              from <- setTriggererFromEmailAddress
              webHost <- setWebHost
              pure
                TriggeredEmailConverterSettings
                  { triggeredEmailConverterSetFromAddress = from,
                    triggeredEmailConverterSetFromName = "Tickler Triggerer",
                    triggeredEmailConverterSetWebHost = webHost
                  }
        ],
        [ mkLooperDef "AdminNotificationEmailConverter" setTriggeredEmailConverterSets
            $ runAdminNotificationEmailConverter adminNotificationEmailConverterSettings
          | adminNotificationEmailConverterSettings <- maybeToList $ do
              from <- setAdminNotificationFromEmailAddress
              to <- setAdminNotificationToEmailAddress
              webHost <- setWebHost
              pure
                AdminNotificationEmailConverterSettings
                  { adminNotificationEmailConverterSetFromAddress = from,
                    adminNotificationEmailConverterSetFromName = "Tickler Admin Notification",
                    adminNotificationEmailConverterSetToAddress = to,
                    adminNotificationEmailConverterSetToName = "Tickler Admin",
                    adminNotificationEmailConverterSetWebHost = webHost
                  }
        ]
      ]

addLooperNameToLog :: Text -> LoggingT m a -> LoggingT m a
addLooperNameToLog looperName = modLogSource $ \source -> if source == "" then looperName else source

modLogSource :: (LogSource -> LogSource) -> LoggingT m a -> LoggingT m a
modLogSource func (LoggingT mFunc) = LoggingT $ \logFunc ->
  let newLogFunc loc source level str =
        let source' = func source
         in logFunc loc source' level str
   in mFunc newLogFunc
