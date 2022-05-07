module Tickler.Server.Looper.TriggeredIntrayItemScheduler
  ( runTriggeredIntrayItemScheduler,
    scheduleTriggeredIntrayItem,
  )
where

import Conduit
import qualified Data.Conduit.Combinators as C
import qualified Data.Text as T
import Database.Persist.Sqlite
import Import
import Tickler.Data
import Tickler.Server.Looper.DB
import Tickler.Server.Looper.Types

runTriggeredIntrayItemScheduler :: Looper ()
runTriggeredIntrayItemScheduler = do
  acqTriggeredItemsSource <- runDb $ selectSourceRes [] [Desc TriggeredItemTriggered]
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
  logDebugN $
    T.pack $
      unwords
        [ "Considering scheduling a triggered intray item for triggered item with identifier",
          uuidString $ triggeredItemIdentifier ti
        ]
  mte <-
    runDb $
      getBy $
        UniqueTriggeredIntrayItem (triggeredItemIdentifier ti) (userTriggerTriggerId ut)
  case mte of
    Nothing -> do
      logInfoN $
        T.pack $
          unwords
            [ "Scheduling a triggered intray item for triggered item with identifier",
              uuidString $ triggeredItemIdentifier ti
            ]
      runDb $
        insert_
          TriggeredIntrayItem
            { triggeredIntrayItemItem = triggeredItemIdentifier ti,
              triggeredIntrayItemTrigger = userTriggerTriggerId ut,
              triggeredIntrayItemIntrayItemUUID = Nothing,
              triggeredIntrayItemError = Nothing
            }
    Just _ -> pure ()
