{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

module Tickler.Server.Looper.Triggerer where

import Import

import Control.Monad.Logger
import Data.Time
import Database.Persist.Sqlite

import Tickler.API

import Tickler.Server.OptParse.Types

import Tickler.Server.Looper.DB
import Tickler.Server.Looper.Types

runTriggerer :: TriggererSettings -> Looper ()
runTriggerer TriggererSettings = do
  logInfoNS "Triggerer" "Starting triggering tickles."
  nowZoned <- liftIO getZonedTime
  let now = zonedTimeToUTC nowZoned
      nowLocal = zonedTimeToLocalTime nowZoned
      nowDay = localDay nowLocal
      later = addDays 2 nowDay
  runDb $ do
    itemsToConsider <-
      selectList
        [TicklerItemScheduledDay <=. later]
        [Asc TicklerItemScheduledDay, Asc TicklerItemScheduledTime]
    items <-
      flip filterM itemsToConsider $ \(Entity _ ti@TicklerItem {..}) -> do
        mSets <- getBy $ UniqueUserSettings ticklerItemUserId
        let tz = maybe utc (userSettingsTimeZone . entityVal) mSets
        pure $ shouldBeTriggered now tz ti -- utcTimeInUserTimezone <= now
    forM_ items $ \(Entity tii ti) -> do
      insert_ $ makeTriggeredItem now ti -- Make the triggered item
      delete tii -- Delete the tickler item
      nti <- liftIO $ makeNextTickleItem ti
      forM nti insert_ -- Insert the next tickler item if necessary
  logInfoNS "Triggerer" "Finished triggering tickles."

shouldBeTriggered :: UTCTime -> TimeZone -> TicklerItem -> Bool
shouldBeTriggered now tz ti = localTimeToUTC tz (ticklerItemLocalScheduledTime ti) <= now

ticklerItemLocalScheduledTime :: TicklerItem -> LocalTime
ticklerItemLocalScheduledTime TicklerItem {..} =
  LocalTime ticklerItemScheduledDay $ fromMaybe midnight ticklerItemScheduledTime

makeTriggeredItem :: UTCTime -> TicklerItem -> TriggeredItem
makeTriggeredItem now TicklerItem {..} =
  TriggeredItem
    { triggeredItemIdentifier = ticklerItemIdentifier
    , triggeredItemUserId = ticklerItemUserId
    , triggeredItemType = ticklerItemType
    , triggeredItemContents = ticklerItemContents
    , triggeredItemCreated = ticklerItemCreated
    , triggeredItemScheduledDay = ticklerItemScheduledDay
    , triggeredItemScheduledTime = ticklerItemScheduledTime
    , triggeredItemRecurrence = ticklerItemRecurrence
    , triggeredItemTriggered = now
    }

nextScheduledTime :: Day -> Maybe TimeOfDay -> Recurrence -> (Day, Maybe TimeOfDay)
nextScheduledTime scheduledDay _ r =
  case r of
    EveryDaysAtTime ds mtod -> (addDays (fromIntegral ds) scheduledDay, mtod)
    EveryMonthsOnDay ms md mtod ->
      let clipped = addGregorianMonthsClip (fromIntegral ms) scheduledDay
          day =
            case md of
              Nothing -> clipped
              Just d_ ->
                let (y, m, _) = toGregorian clipped
                 in fromGregorian y m (fromIntegral d_)
       in (day, mtod)

makeNextTickleItem :: TicklerItem -> IO (Maybe TicklerItem)
makeNextTickleItem ti =
  case ticklerItemRecurrence ti of
    Nothing -> pure Nothing
    Just r ->
      fmap Just $ do
        let (d, mtod) =
              nextScheduledTime (ticklerItemScheduledDay ti) (ticklerItemScheduledTime ti) r
        uuid <- nextRandomUUID
        pure $
          ti
            { ticklerItemIdentifier = uuid
            , ticklerItemScheduledDay = d
            , ticklerItemScheduledTime = mtod
            }
