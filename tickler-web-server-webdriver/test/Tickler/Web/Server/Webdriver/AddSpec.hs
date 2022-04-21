{-# LANGUAGE OverloadedStrings #-}

module Tickler.Web.Server.Webdriver.AddSpec (spec) where

import Tickler.Web.Server.Webdriver.TestImport

spec :: WebdriverSpec App
spec = do
  it "can add a tickle without recurrence" $
    addSpec
      Tickle
        { tickleContent = "hello world",
          tickleScheduledDay = fromGregorian 2222 04 21,
          tickleScheduledTime = Nothing,
          tickleRecurrence = Nothing
        }

  it "can add a tickle without recurrence with a scheduled time" $
    addSpec
      Tickle
        { tickleContent = "hello world",
          tickleScheduledDay = fromGregorian 2222 04 21,
          tickleScheduledTime = Just $ TimeOfDay 12 45 00,
          tickleRecurrence = Nothing
        }

  it "can add a tickle with daily recurrence" $
    addSpec
      Tickle
        { tickleContent = "hello world",
          tickleScheduledDay = fromGregorian 2222 04 21,
          tickleScheduledTime = Just $ TimeOfDay 12 45 00,
          tickleRecurrence =
            Just $ EveryDaysAtTime 5 Nothing
        }

  it "can add a tickle with daily recurrence and time of day" $
    addSpec
      Tickle
        { tickleContent = "hello world",
          tickleScheduledDay = fromGregorian 2222 04 21,
          tickleScheduledTime = Just $ TimeOfDay 12 45 00,
          tickleRecurrence =
            Just $ EveryDaysAtTime 5 (Just (TimeOfDay 12 34 00))
        }

  it "can add a tickle with monthly recurrence" $
    addSpec
      Tickle
        { tickleContent = "hello world",
          tickleScheduledDay = fromGregorian 2222 04 21,
          tickleScheduledTime = Just $ TimeOfDay 12 45 00,
          tickleRecurrence =
            Just $ EveryMonthsOnDay 6 Nothing Nothing
        }

  it "can add a tickle with monthly recurrence and a specific day" $
    addSpec
      Tickle
        { tickleContent = "hello world",
          tickleScheduledDay = fromGregorian 2222 04 21,
          tickleScheduledTime = Just $ TimeOfDay 12 45 00,
          tickleRecurrence =
            Just $ EveryMonthsOnDay 6 (Just 15) Nothing
        }

  it "can add a tickle with monthly recurrence and a specific time" $
    addSpec
      Tickle
        { tickleContent = "hello world",
          tickleScheduledDay = fromGregorian 2222 04 21,
          tickleScheduledTime = Just $ TimeOfDay 12 45 00,
          tickleRecurrence =
            Just $ EveryMonthsOnDay 6 Nothing (Just (TimeOfDay 23 45 00))
        }

  it "can add a tickle with monthly recurrence and a specific day and time" $
    addSpec
      Tickle
        { tickleContent = "hello world",
          tickleScheduledDay = fromGregorian 2222 04 21,
          tickleScheduledTime = Just $ TimeOfDay 12 45 00,
          tickleRecurrence =
            Just $ EveryMonthsOnDay 6 (Just 15) (Just (TimeOfDay 23 45 00))
        }

addSpec :: Tickle -> WebdriverTestM App ()
addSpec tickle = do
  driveAsNewUser dummyUser $ do
    uuid <- driveAddTickle tickle
    liftIO $ shouldBeValid uuid

-- TODO get the tickle out of the DB to check that it's correct.
