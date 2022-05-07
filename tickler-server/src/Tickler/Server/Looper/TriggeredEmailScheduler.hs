{-# LANGUAGE RecordWildCards #-}

module Tickler.Server.Looper.TriggeredEmailScheduler
  ( runTriggeredEmailScheduler,
  )
where

import Conduit
import qualified Data.Conduit.Combinators as C
import qualified Data.Text as T
import Database.Persist
import Import
import Tickler.Data
import Tickler.Server.Looper.DB
import Tickler.Server.Looper.Types

runTriggeredEmailScheduler :: Looper ()
runTriggeredEmailScheduler = do
  acqTriggeredItemsSource <- runDb $ selectSourceRes [] [Asc TriggeredItemScheduledDay, Asc TriggeredItemScheduledTime]
  withAcquire acqTriggeredItemsSource $ \triggeredItemsSource ->
    runConduit $ triggeredItemsSource .| C.mapM_ scheduleTriggeredEmail

scheduleTriggeredEmail :: Entity TriggeredItem -> Looper ()
scheduleTriggeredEmail (Entity _ ti) = do
  acqUserTriggersSource <-
    runDb $
      selectSourceRes
        [ UserTriggerUserId ==. triggeredItemUserId ti,
          UserTriggerTriggerType ==. EmailTriggerType
        ]
        []
  withAcquire acqUserTriggersSource $ \userTriggersSource ->
    runConduit $ userTriggersSource .| C.mapM_ (scheduleTriggeredEmailWithUserTrigger ti)

scheduleTriggeredEmailWithUserTrigger :: TriggeredItem -> Entity UserTrigger -> Looper ()
scheduleTriggeredEmailWithUserTrigger ti (Entity _ ut) = do
  logDebugN $
    T.pack $
      unwords
        [ "Considering scheduling a triggered email item for triggered item with identifier",
          uuidString $ triggeredItemIdentifier ti
        ]
  met <- runDb $ selectFirst [EmailTriggerIdentifier ==. userTriggerTriggerId ut] []
  case met of
    Nothing -> pure ()
    Just (Entity _ EmailTrigger {..}) ->
      if emailTriggerVerified
        then do
          mte <-
            runDb $
              getBy $
                UniqueTriggeredEmail (triggeredItemIdentifier ti) (userTriggerTriggerId ut)
          case mte of
            Nothing -> do
              logInfoN $
                T.pack $
                  unwords
                    [ "Scheduling a triggered email item for triggered item with identifier",
                      uuidString $ triggeredItemIdentifier ti
                    ]
              runDb $
                insert_
                  TriggeredEmail
                    { triggeredEmailItem = triggeredItemIdentifier ti,
                      triggeredEmailTrigger = userTriggerTriggerId ut,
                      triggeredEmailEmail = Nothing,
                      triggeredEmailError = Nothing
                    }
            Just _ -> pure ()
        else pure ()
