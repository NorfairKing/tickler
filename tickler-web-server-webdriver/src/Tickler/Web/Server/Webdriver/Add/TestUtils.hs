{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Tickler.Web.Server.Webdriver.Add.TestUtils where

import Control.Monad
import Data.List as List ((\\))
import Data.Maybe
import qualified Data.Text as T
import Data.Time
import Test.Syd
import Test.Syd.Webdriver
import Test.WebDriver
import Tickler.API
import Tickler.Client
import Tickler.Web.Server.Foundation
import Tickler.Web.Server.Webdriver.Auth.TestUtils
import Tickler.Web.Server.Webdriver.TestUtils

driveAddTickle :: TestUser -> Tickle -> WebdriverTestM App ItemUUID
driveAddTickle user tickle = do
  token <- loginViaAPI user

  itemsBefore <- driveClientOrErr $ clientGetItems token

  -- Add the item
  findElem (ById "nav-add") >>= click
  driveFillInTickleForm tickle
  findElem (ById "submit") >>= submit

  itemsAfter <- driveClientOrErr $ clientGetItems token

  -- Find the item that was added
  liftIO $ case itemsAfter List.\\ itemsBefore of
    [ItemInfo {..}] -> pure itemInfoIdentifier
    _ -> expectationFailure "Expected to find exactly one new item."

driveFillInTickleForm :: Tickle -> WebdriverTestM App ()
driveFillInTickleForm Tickle {..} = do
  -- Contents
  findElem (ByName "contents") >>= sendKeys tickleContent

  -- Scheduled day
  findElem (ByName "scheduled-day")
    >>= sendKeys (T.pack (formatTime defaultTimeLocale "%m%d%Y" tickleScheduledDay))

  -- Recurrence
  case tickleRecurrence of
    Nothing -> do
      -- Scheduled time
      forM_ tickleScheduledTime $ \scheduledTime ->
        findElem (ByName "scheduled-time")
          >>= sendKeys (T.pack (formatTime defaultTimeLocale "%I%M%p" scheduledTime))
    Just r -> case r of
      -- Every day
      EveryDaysAtTime ds mtod -> do
        findElem (ById "EveryDay") >>= click

        -- Number of days
        findElem (ByName "days") >>= sendKeys (T.pack (show ds))

        -- Time of day
        forM_ mtod $ \tod ->
          findElem (ByName "day-time-of-day")
            >>= sendKeys (T.pack (formatTime defaultTimeLocale "%I%M%p" tod))

      -- Every month
      EveryMonthsOnDay ms md mtod -> do
        findElem (ById "EveryMonth") >>= click

        -- Number of months
        findElem (ByName "months") >>= sendKeys (T.pack (show ms))

        -- Day within the month
        forM_ md $ \d ->
          findElem (ByName "day")
            >>= sendKeys (T.pack (show d))

        -- Time of day
        forM_ mtod $ \tod ->
          findElem (ByName "month-time-of-day")
            >>= sendKeys (T.pack (formatTime defaultTimeLocale "%I%M%p" tod))

dummyTickle :: Tickle
dummyTickle = head dummyTickles

dummyTickles :: [Tickle]
dummyTickles = do
  tickleScheduledDay <- [fromGregorian 2222 04 24]
  (tickleRecurrence, tickleScheduledTime) <-
    mconcat
      [ do
          mtod <- [Nothing, Just $ timeOfDayToMinuteOfDay $ TimeOfDay 12 15 00]
          pure (Nothing, mtod),
        do
          mtod <- [Nothing, Just $ timeOfDayToMinuteOfDay $ TimeOfDay 12 30 00]
          pure (Just $ EveryDaysAtTime 5 mtod, Nothing),
        do
          mtod <- [Nothing, Just $ timeOfDayToMinuteOfDay $ TimeOfDay 12 45 00]
          md <- [Nothing, Just 15]
          pure (Just $ EveryMonthsOnDay 6 md mtod, Nothing)
      ]
  let tickleContent =
        T.pack $
          case tickleRecurrence of
            Nothing ->
              if isJust tickleScheduledTime
                then "scheduled time, "
                else "no scheduled time, "
            Just recurrence -> case recurrence of
              EveryDaysAtTime _ mtod ->
                mconcat
                  [ "daily recurrence, ",
                    if isJust mtod
                      then "time of day"
                      else "no time of day"
                  ]
              EveryMonthsOnDay _ md mtod ->
                mconcat
                  [ "monthly recurrence, ",
                    if isJust md
                      then "day, "
                      else "no day, ",
                    if isJust mtod
                      then "time of day"
                      else "no time of day"
                  ]
  pure Tickle {..}
