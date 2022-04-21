{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Tickler.Web.Server.Webdriver.Add.TestUtils where

import Control.Monad
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

dummyTickles :: [Tickle]
dummyTickles =
  [ Tickle
      { tickleContent = "hello world",
        tickleScheduledDay = fromGregorian 2222 04 21,
        tickleScheduledTime = Nothing,
        tickleRecurrence = Nothing
      },
    Tickle
      { tickleContent = "hello world",
        tickleScheduledDay = fromGregorian 2222 04 21,
        tickleScheduledTime = Just $ TimeOfDay 12 45 00,
        tickleRecurrence = Nothing
      },
    Tickle
      { tickleContent = "hello world",
        tickleScheduledDay = fromGregorian 2222 04 21,
        tickleScheduledTime = Just $ TimeOfDay 12 45 00,
        tickleRecurrence =
          Just $ EveryDaysAtTime 5 Nothing
      },
    Tickle
      { tickleContent = "hello world",
        tickleScheduledDay = fromGregorian 2222 04 21,
        tickleScheduledTime = Just $ TimeOfDay 12 45 00,
        tickleRecurrence =
          Just $ EveryDaysAtTime 5 (Just (TimeOfDay 12 34 00))
      },
    Tickle
      { tickleContent = "hello world",
        tickleScheduledDay = fromGregorian 2222 04 21,
        tickleScheduledTime = Just $ TimeOfDay 12 45 00,
        tickleRecurrence =
          Just $ EveryMonthsOnDay 6 Nothing Nothing
      },
    Tickle
      { tickleContent = "hello world",
        tickleScheduledDay = fromGregorian 2222 04 21,
        tickleScheduledTime = Just $ TimeOfDay 12 45 00,
        tickleRecurrence =
          Just $ EveryMonthsOnDay 6 (Just 15) Nothing
      },
    Tickle
      { tickleContent = "hello world",
        tickleScheduledDay = fromGregorian 2222 04 21,
        tickleScheduledTime = Just $ TimeOfDay 12 45 00,
        tickleRecurrence =
          Just $ EveryMonthsOnDay 6 Nothing (Just (TimeOfDay 23 45 00))
      },
    Tickle
      { tickleContent = "hello world",
        tickleScheduledDay = fromGregorian 2222 04 21,
        tickleScheduledTime = Just $ TimeOfDay 12 45 00,
        tickleRecurrence =
          Just $ EveryMonthsOnDay 6 (Just 15) (Just (TimeOfDay 23 45 00))
      }
  ]
