module Tickler.Server.Looper.TriggeredIntrayItemScheduler
  ( runTriggeredIntrayItemScheduler,
  )
where

import Conduit
import qualified Data.Conduit.Combinators as C
import qualified Data.Text as T
import Database.Esqueleto.Legacy
import Import
import Tickler.Data
import Tickler.Server.Looper.DB
import Tickler.Server.Looper.Types

runTriggeredIntrayItemScheduler :: Looper ()
runTriggeredIntrayItemScheduler = do
  let source =
        selectSource $
          from $ \((triggeredItem `InnerJoin` user) `CrossJoin` intrayTrigger) -> do
            where_ (user ^. UserIdentifier ==. intrayTrigger ^. IntrayTriggerUser)
            on (triggeredItem ^. TriggeredItemUserId ==. user ^. UserIdentifier)
            where_ $
              notExists $
                from $ \triggeredIntrayItem ->
                  where_ $
                    (triggeredIntrayItem ^. TriggeredIntrayItemTrigger ==. intrayTrigger ^. IntrayTriggerIdentifier)
                      &&. (triggeredIntrayItem ^. TriggeredIntrayItemItem ==. triggeredItem ^. TriggeredItemIdentifier)
            pure (triggeredItem, intrayTrigger)

  runDB $ runConduit $ source .| C.mapM_ (uncurry scheduleTriggeredIntrayItemViaIntrayTrigger)

scheduleTriggeredIntrayItemViaIntrayTrigger :: (MonadIO m, MonadLogger m) => Entity TriggeredItem -> Entity IntrayTrigger -> SqlPersistT m ()
scheduleTriggeredIntrayItemViaIntrayTrigger (Entity _ ti) (Entity _ it) = do
  logInfoN $
    T.pack $
      unwords
        [ "Scheduling a triggered intray item for triggered item with identifier",
          uuidString $ triggeredItemIdentifier ti
        ]
  insert_
    TriggeredIntrayItem
      { triggeredIntrayItemItem = triggeredItemIdentifier ti,
        triggeredIntrayItemTrigger = intrayTriggerIdentifier it,
        triggeredIntrayItemIntrayItemUUID = Nothing,
        triggeredIntrayItemError = Nothing
      }
