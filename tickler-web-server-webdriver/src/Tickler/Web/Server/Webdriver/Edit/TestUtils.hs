{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Tickler.Web.Server.Webdriver.Edit.TestUtils where

import Control.Monad
import qualified Data.Text as T
import Data.Time
import Test.Syd.Webdriver
import Test.WebDriver
import Tickler.API
import Tickler.Web.Server.Foundation

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

  -- Recurrence
  case tickleRecurrence of
    -- No recurrence
    Nothing -> do
      findElem (ById "None") >>= click

      -- Scheduled time of day
      scheduledTimeE <- findElem (ByName "scheduled-time")
      clearInput scheduledTimeE
      forM_ tickleScheduledTime $ \scheduledTime -> do
        sendKeys (T.pack (formatTime defaultTimeLocale "%I%M%p" scheduledTime)) scheduledTimeE
    Just recurrence -> case recurrence of
      -- Daily recurrence
      EveryDaysAtTime ds mtod -> do
        findElem (ById "EveryDay") >>= click

        -- Number of days
        daysE <- findElem (ByName "days")
        clearInput daysE
        sendKeys (T.pack (show ds)) daysE

        -- Time of day
        todE <- findElem (ByName "day-time-of-day")
        clearInput todE
        forM_ mtod $ \tod -> do
          sendKeys (T.pack (formatTime defaultTimeLocale "%I%M%p" tod)) todE

      -- Monthly recurrence
      EveryMonthsOnDay ms md mtod -> do
        -- Monthly recurrence
        findElem (ById "EveryMonth") >>= click

        -- Number of months
        monthsE <- findElem (ByName "months")
        clearInput monthsE
        sendKeys (T.pack (show ms)) monthsE

        -- Day within the month
        dayE <- findElem (ByName "day")
        clearInput dayE
        forM_ md $ \d -> do
          sendKeys (T.pack (show d)) dayE

        -- Time of day
        todE <- findElem (ByName "month-time-of-day")
        clearInput todE
        forM_ mtod $ \tod -> do
          sendKeys (T.pack (formatTime defaultTimeLocale "%I%M%p" tod)) todE
