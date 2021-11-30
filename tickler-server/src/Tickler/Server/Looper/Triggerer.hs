{-# LANGUAGE RecordWildCards #-}

module Tickler.Server.Looper.Triggerer where

import Conduit
import qualified Data.Conduit.Combinators as C
import Data.Time
import Database.Persist.Sqlite
import Import
import Tickler.API
import Tickler.Server.Looper.DB
import Tickler.Server.Looper.Types

runTriggerer :: () -> Looper ()
runTriggerer () = do
  nowZoned <- liftIO getZonedTime
  let nowLocal = zonedTimeToLocalTime nowZoned
      nowDay = localDay nowLocal
      later = addDays 2 nowDay
  acqItemsToConsiderSource <-
    runDb $
      selectSourceRes
        [TicklerItemScheduledDay <=. later]
        [Asc TicklerItemScheduledDay, Asc TicklerItemScheduledTime]
  withAcquire acqItemsToConsiderSource $ \itemsToConsiderSource ->
    runConduit $ itemsToConsiderSource .| C.mapM_ considerTicklerItem

considerTicklerItem :: Entity TicklerItem -> Looper ()
considerTicklerItem e@(Entity _ ti@TicklerItem {..}) =
  runDb $ do
    now <- liftIO getCurrentTime
    mSets <- getBy $ UniqueUserSettings ticklerItemUserId
    let tz = maybe utc (userSettingsTimeZone . entityVal) mSets
    when (shouldBeTriggered now tz ti) $ triggerTicklerItem now e

triggerTicklerItem :: UTCTime -> Entity TicklerItem -> SqlPersistT IO ()
triggerTicklerItem now (Entity tii ti) = do
  uuid <- nextRandomUUID
  insert_ $ makeTriggeredItem uuid now ti -- Make the triggered item
  case ticklerItemUpdates ti of
    Nothing -> delete tii -- Delete the tickler item
    Just updates -> update tii updates

shouldBeTriggered :: UTCTime -> TimeZone -> TicklerItem -> Bool
shouldBeTriggered now tz ti = localTimeToUTC tz (ticklerItemLocalScheduledTime ti) <= now

ticklerItemLocalScheduledTime :: TicklerItem -> LocalTime
ticklerItemLocalScheduledTime TicklerItem {..} =
  LocalTime ticklerItemScheduledDay $ fromMaybe midnight ticklerItemScheduledTime

makeTriggeredItem :: ItemUUID -> UTCTime -> TicklerItem -> TriggeredItem
makeTriggeredItem uuid now TicklerItem {..} =
  TriggeredItem
    { triggeredItemIdentifier = uuid,
      triggeredItemUserId = ticklerItemUserId,
      triggeredItemType = ticklerItemType,
      triggeredItemContents = ticklerItemContents,
      triggeredItemCreated = ticklerItemCreated,
      triggeredItemScheduledDay = ticklerItemScheduledDay,
      triggeredItemScheduledTime = ticklerItemScheduledTime,
      triggeredItemRecurrence = ticklerItemRecurrence,
      triggeredItemTriggered = now
    }

ticklerItemUpdates :: TicklerItem -> Maybe [Update TicklerItem]
ticklerItemUpdates ti = do
  r <- ticklerItemRecurrence ti
  let (d, mtod) = nextScheduledTime (ticklerItemScheduledDay ti) (ticklerItemScheduledTime ti) r
  pure [TicklerItemScheduledDay =. d, TicklerItemScheduledTime =. mtod]
