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
driveAddTickle Tickle {..} = do
  findElem (ById "nav-add") >>= click

  findElem (ByName "contents") >>= sendKeys tickleContent
  findElem (ByName "scheduled-day")
    >>= sendKeys (T.pack (formatTime defaultTimeLocale "%m%d%Y" tickleScheduledDay))
  forM_ tickleScheduledTime $ \scheduledTime ->
    findElem (ByName "scheduled-time")
      >>= sendKeys (T.pack (formatTime defaultTimeLocale "%I%M%p" scheduledTime))
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

  findElem (ById "submit") >>= submit
  route <- getCurrentRoute
  liftIO $ case route of
    EditR uuid -> pure uuid
    _ -> expectationFailure "Expected to be on an EditR"
