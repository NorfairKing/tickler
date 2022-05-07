{-# LANGUAGE RecordWildCards #-}

module Tickler.Server.Looper.TriggeredEmailScheduler
  ( runTriggeredEmailScheduler,
    scheduleTriggeredEmail,
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
  acqEmailTriggersSource <-
    runDb $
      selectSourceRes
        [EmailTriggerUser ==. Just (triggeredItemUserId ti)]
        []
  withAcquire acqEmailTriggersSource $ \emailTriggersSource ->
    runConduit $ emailTriggersSource .| C.mapM_ (scheduleTriggeredEmailWithEmailTrigger ti)

scheduleTriggeredEmailWithEmailTrigger :: TriggeredItem -> Entity EmailTrigger -> Looper ()
scheduleTriggeredEmailWithEmailTrigger ti (Entity _ EmailTrigger {..}) = do
  logDebugN $
    T.pack $
      unwords
        [ "Considering scheduling a triggered email item for triggered item with identifier",
          uuidString $ triggeredItemIdentifier ti
        ]
  if emailTriggerVerified
    then do
      mte <-
        runDb $
          getBy $
            UniqueTriggeredEmail (triggeredItemIdentifier ti) emailTriggerIdentifier
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
                  triggeredEmailTrigger = emailTriggerIdentifier,
                  triggeredEmailEmail = Nothing,
                  triggeredEmailError = Nothing
                }
        Just _ -> pure ()
    else pure ()
