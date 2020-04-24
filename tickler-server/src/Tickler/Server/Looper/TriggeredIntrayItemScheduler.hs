module Tickler.Server.Looper.TriggeredIntrayItemScheduler
  ( runTriggeredIntrayItemScheduler
  ) where

import Database.Persist.Sqlite
import Import
import Tickler.Data
import Tickler.Server.Looper.DB
import Tickler.Server.Looper.Types

runTriggeredIntrayItemScheduler :: () -> Looper ()
runTriggeredIntrayItemScheduler () = do
  tis <- runDb $ selectList [] [Asc TriggeredItemScheduledDay, Asc TriggeredItemScheduledTime]
  tes <-
    fmap concat $
    forM tis $ \(Entity _ ti) -> do
      uts <-
        runDb $
        selectList
          [ UserTriggerUserId ==. triggeredItemUserId ti
          , UserTriggerTriggerType ==. IntrayTriggerType
          ]
          []
      fmap catMaybes $
        forM uts $ \(Entity _ ut) -> do
          mte <-
            runDb $
            getBy $ UniqueTriggeredIntrayItem (triggeredItemIdentifier ti) (userTriggerTriggerId ut)
          pure $
            case mte of
              Nothing ->
                Just
                  TriggeredIntrayItem
                    { triggeredIntrayItemItem = triggeredItemIdentifier ti
                    , triggeredIntrayItemTrigger = userTriggerTriggerId ut
                    , triggeredIntrayItemIntrayItemUUID = Nothing
                    , triggeredIntrayItemError = Nothing
                    }
              Just _ -> Nothing
  runDb $ insertMany_ tes
