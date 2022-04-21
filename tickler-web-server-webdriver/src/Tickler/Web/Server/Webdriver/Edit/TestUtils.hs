{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Tickler.Web.Server.Webdriver.Edit.TestUtils where

import Control.Monad
import qualified Data.Text as T
import Data.Time
import Test.Syd
import Test.Syd.Webdriver
import Test.Syd.Webdriver.Yesod
import Test.WebDriver
import Tickler.API
import Tickler.Web.Server.Foundation
import Tickler.Web.Server.Webdriver.Add.TestUtils

driveEditTickle :: ItemUUID -> Tickle -> WebdriverTestM App ()
driveEditTickle uuid tickle = do
  findElem (ById "nav-tickles") >>= click
  findElem (ById $ uuidText uuid) >>= \e -> findElemFrom e (ById "edit") >>= click
  driveEditTickleForm tickle
  findElem (ById "submit") >>= click

driveEditTickleForm :: Tickle -> WebdriverTestM App ()
driveEditTickleForm Tickle {..} = do
  -- Contents
  contentsE <- findElem (ByName "contents")
  clearInput contentsE
  sendKeys tickleContent contentsE

  -- Scheduled day
  scheduledDayE <- findElem (ByName "scheduled-day")
  clearInput scheduledDayE
  sendKeys (T.pack (formatTime defaultTimeLocale "%m%d%Y" tickleScheduledDay)) scheduledDayE

  -- Scheduled time of day
  forM_ tickleScheduledTime $ \scheduledTime -> do
    scheduledTimeE <- findElem (ByName "scheduled-time")
    clearInput scheduledTimeE
    sendKeys (T.pack (formatTime defaultTimeLocale "%I%M%p" scheduledTime)) scheduledTimeE

  -- Recurrence
  forM_ tickleRecurrence $ \case
    EveryDaysAtTime ds mtod -> do
      findElem (ById "EveryDay") >>= click
      findElem (ByName "days") >>= sendKeys (T.pack (show ds))
      forM_ mtod $ \tod ->
        findElem (ByName "day-time-of-day")
          >>= sendKeys (T.pack (formatTime defaultTimeLocale "%I%M%p" tod))
    EveryMonthsOnDay ms md mtod -> do
      findElem (ById "EveryMonth") >>= click
      findElem (ByName "months") >>= sendKeys (T.pack (show ms))
      forM_ md $ \d ->
        findElem (ByName "day")
          >>= sendKeys (T.pack (show d))
      forM_ mtod $ \tod ->
        findElem (ByName "month-time-of-day")
          >>= sendKeys (T.pack (formatTime defaultTimeLocale "%I%M%p" tod))

-- Tickle with known-good contents, time and recurrence for webdriver tests
dummyTickle :: Tickle
dummyTickle =
  Tickle
    { tickleContent = "hello world",
      tickleScheduledDay = fromGregorian 2222 04 21,
      tickleScheduledTime = Just $ TimeOfDay 12 45 00,
      tickleRecurrence =
        Just $ EveryMonthsOnDay 6 (Just 15) (Just (TimeOfDay 23 45 00))
    }
