{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Tickler.Web.Server.Webdriver.Add.TestUtils where

import Control.Monad
import Data.Maybe
import qualified Data.Text as T
import Data.Time
import Test.Syd
import Test.Syd.Webdriver
import Test.Syd.Webdriver.Yesod
import Test.WebDriver
import Tickler.API
import Tickler.Web.Server.Foundation

driveAddTickle :: Tickle -> WebdriverTestM App ItemUUID
driveAddTickle tickle = do
  findElem (ById "nav-add") >>= click
  driveFillInTickleForm tickle
  findElem (ById "submit") >>= submit
  route <- getCurrentRoute
  liftIO $ case route of
    EditR uuid -> pure uuid
    _ -> expectationFailure "Expected to be on an EditR"

driveFillInTickleForm :: Tickle -> WebdriverTestM App ()
driveFillInTickleForm Tickle {..} = do
  -- Contents
  findElem (ByName "contents") >>= sendKeys tickleContent

  -- Scheduled day
  findElem (ByName "scheduled-day")
    >>= sendKeys (T.pack (formatTime defaultTimeLocale "%m%d%Y" tickleScheduledDay))

  -- Scheduled time
  forM_ tickleScheduledTime $ \scheduledTime ->
    findElem (ByName "scheduled-time")
      >>= sendKeys (T.pack (formatTime defaultTimeLocale "%I%M%p" scheduledTime))

  -- Recurrence
  forM_ tickleRecurrence $ \case
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
  tickleScheduledTime <- [Nothing, Just $ TimeOfDay 12 15 00]
  tickleRecurrence <-
    mconcat
      [ [Nothing],
        do
          mtod <- [Nothing, Just $ TimeOfDay 12 30 00]
          pure $ Just $ EveryDaysAtTime 5 mtod,
        do
          mtod <- [Nothing, Just $ TimeOfDay 12 45 00]
          md <- [Nothing, Just 15]
          pure $ Just $ EveryMonthsOnDay 6 md mtod
      ]
  let tickleContent =
        T.pack $
          mconcat
            [ if isJust tickleScheduledTime
                then "scheduled time, "
                else "no scheduled time, ",
              case tickleRecurrence of
                Nothing -> "no recurrence"
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
            ]
  pure Tickle {..}
