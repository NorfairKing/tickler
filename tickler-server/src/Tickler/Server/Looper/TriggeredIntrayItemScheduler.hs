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
  acqIntrayTriggersSource <-
    runDb $
      selectSourceRes
        [IntrayTriggerUser ==. triggeredItemUserId ti]
        []

  withAcquire acqIntrayTriggersSource $ \intrayTriggersSource ->
    runConduit $ intrayTriggersSource .| C.mapM_ (scheduleTriggeredIntrayItemViaIntrayTrigger ti)

scheduleTriggeredIntrayItemViaIntrayTrigger :: TriggeredItem -> Entity IntrayTrigger -> Looper ()
scheduleTriggeredIntrayItemViaIntrayTrigger ti (Entity _ it) = do
  logDebugN $
    T.pack $
      unwords
        [ "Considering scheduling a triggered intray item for triggered item with identifier",
          uuidString $ triggeredItemIdentifier ti
        ]
  mte <-
    runDb $
      getBy $
        UniqueTriggeredIntrayItem (triggeredItemIdentifier ti) (intrayTriggerIdentifier it)
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
              triggeredIntrayItemTrigger = intrayTriggerIdentifier it,
              triggeredIntrayItemIntrayItemUUID = Nothing,
              triggeredIntrayItemError = Nothing
            }
    Just _ -> pure ()
