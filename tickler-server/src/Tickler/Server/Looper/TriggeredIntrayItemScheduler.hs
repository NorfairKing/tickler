module Tickler.Server.Looper.TriggeredIntrayItemScheduler
  ( runTriggeredIntrayItemScheduler,
  )
where

import Conduit
import qualified Data.Conduit.Combinators as C
import Database.Persist.Sqlite
import Import
import Tickler.Data
import Tickler.Server.Looper.DB
import Tickler.Server.Looper.Types

runTriggeredIntrayItemScheduler :: Looper ()
runTriggeredIntrayItemScheduler = do
  acqTriggeredItemsSource <- runDb $ selectSourceRes [] [Asc TriggeredItemScheduledDay, Asc TriggeredItemScheduledTime]
  withAcquire acqTriggeredItemsSource $ \triggeredItemsSource ->
    runConduit $ triggeredItemsSource .| C.mapM_ scheduleTriggeredIntrayItem

scheduleTriggeredIntrayItem :: Entity TriggeredItem -> Looper ()
scheduleTriggeredIntrayItem (Entity _ ti) = do
  acqUserIntrayTriggersSource <-
    runDb $
      selectSourceRes
        [ UserTriggerUserId ==. triggeredItemUserId ti,
          UserTriggerTriggerType ==. IntrayTriggerType
        ]
        []
  withAcquire acqUserIntrayTriggersSource $ \userIntrayTriggersSource ->
    runConduit $ userIntrayTriggersSource .| C.mapM_ (scheduleTriggeredIntrayItemViaUserTrigger ti)

scheduleTriggeredIntrayItemViaUserTrigger :: TriggeredItem -> Entity UserTrigger -> Looper ()
scheduleTriggeredIntrayItemViaUserTrigger ti (Entity _ ut) = do
  mte <-
    runDb $
      getBy $
        UniqueTriggeredIntrayItem (triggeredItemIdentifier ti) (userTriggerTriggerId ut)
  case mte of
    Nothing ->
      runDb $
        insert_
          TriggeredIntrayItem
            { triggeredIntrayItemItem = triggeredItemIdentifier ti,
              triggeredIntrayItemTrigger = userTriggerTriggerId ut,
              triggeredIntrayItemIntrayItemUUID = Nothing,
              triggeredIntrayItemError = Nothing
            }
    Just _ -> pure ()
