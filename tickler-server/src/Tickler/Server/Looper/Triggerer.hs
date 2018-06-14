{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Tickler.Server.Looper.Triggerer
    ( runTriggerer
    , makeTriggeredItem
    , nextScheduledTime
    , makeNextTickleItem
    ) where

import Import

import Control.Monad.Logger
import Data.Time
import Database.Persist.Sqlite

import Tickler.API
import Tickler.Data

import Tickler.Server.OptParse.Types

import Tickler.Server.Looper.Types
import Tickler.Server.Looper.Utils

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
            flip filterM itemsToConsider $ \(Entity _ TicklerItem {..}) -> do
                mSets <- getBy $ UniqueUserSettings ticklerItemUserId
                let tz =
                        fromMaybe utc $
                        (userSettingsTimeZone . entityVal) <$> mSets
                let utcTimeInUserTimezone =
                        localTimeToUTC tz $
                        LocalTime ticklerItemScheduledDay $
                        fromMaybe midnight ticklerItemScheduledTime
                pure $ utcTimeInUserTimezone <= now
        unless (null items) $ do
            let (trigIs, tickIs) =
                    unzip $
                    flip map items $ \(Entity _ ti) ->
                        (makeTriggeredItem now ti, makeNextTickleItem ti)
            insertMany_ trigIs
            let newTicks = catMaybes tickIs
            unless (null newTicks) $ insertMany_ newTicks
            -- TODO if something goes wrong here, we should rollback the transaction
            deleteWhere [TicklerItemId <-. map entityKey items]
    logInfoNS "Triggerer" "Finished triggering tickles."

makeTriggeredItem :: UTCTime -> TicklerItem -> TriggeredItem
makeTriggeredItem now TicklerItem {..} =
    TriggeredItem
    { triggeredItemIdentifier = ticklerItemIdentifier
    , triggeredItemUserId = ticklerItemUserId
    , triggeredItemType = ticklerItemType
    , triggeredItemContents = ticklerItemContents
    , triggeredItemCreated = ticklerItemCreated
    , triggeredItemSynced = ticklerItemSynced
    , triggeredItemScheduledDay = ticklerItemScheduledDay
    , triggeredItemScheduledTime = ticklerItemScheduledTime
    , triggeredItemRecurrence = ticklerItemRecurrence
    , triggeredItemTriggered = now
    }

nextScheduledTime ::
       Day -> Maybe TimeOfDay -> Recurrence -> (Day, Maybe TimeOfDay)
nextScheduledTime scheduledDay scheduledTime r =
    case r of
        EveryDaysAtTime ds mtod ->
            (addDays (fromIntegral ds) scheduledDay, mtod)
        EveryMonthsOnDay ms md mtod ->
            let clipped = addGregorianMonthsClip (fromIntegral ms) scheduledDay
                day =
                    case md of
                        Nothing -> clipped
                        Just d_ ->
                            let (y, m, d) = toGregorian clipped
                            in fromGregorian y m (fromIntegral d_)
            in (day, mtod)

makeNextTickleItem :: TicklerItem -> Maybe TicklerItem
makeNextTickleItem ti = do
    r <- ticklerItemRecurrence ti
    let (d, mtod) =
            nextScheduledTime
                (ticklerItemScheduledDay ti)
                (ticklerItemScheduledTime ti)
                r
    pure $ ti {ticklerItemScheduledDay = d, ticklerItemScheduledTime = mtod}
