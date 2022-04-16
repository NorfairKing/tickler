{-# LANGUAGE OverloadedStrings #-}

module Tickler.Web.Server.Handler.AddSpec where

import Data.Time
import Network.HTTP.Types
import Test.Syd.Yesod
import TestImport
import Tickler.Client
import Tickler.Data
import Tickler.Web.Server.Foundation
import Tickler.Web.Server.Handler.Item
import Tickler.Web.Server.TestUtils

spec :: Spec
spec = do
  describe "mkRecurrence" $ do
    it "works for multiple days" $
      forAllValid $ \(days, todd) ->
        forAllValid $ \(months, day, todm) ->
          case everyDaysAtTime days todd of
            Nothing -> pure ()
            Just _ ->
              mkRecurrence
                RecurrenceData
                  { recurrenceDataOption = Days,
                    recurrenceDataDays = Just days,
                    recurrenceDataDayTimeOfDay = todd,
                    recurrenceDataMonths = months,
                    recurrenceDataMonthDay = day,
                    recurrenceDataMonthTimeOfDay = todm
                  }
                `shouldBe` Just (Just (EveryDaysAtTime days todd))

    it "works for multiple months" $
      forAllValid $ \(months, day, todm) ->
        forAllValid $ \(days, todd) ->
          case everyMonthsOnDayAtTime months day todm of
            Nothing -> pure ()
            Just _ ->
              mkRecurrence
                RecurrenceData
                  { recurrenceDataOption = Months,
                    recurrenceDataDays = days,
                    recurrenceDataDayTimeOfDay = todd,
                    recurrenceDataMonths = Just months,
                    recurrenceDataMonthDay = day,
                    recurrenceDataMonthTimeOfDay = todm
                  }
                `shouldBe` Just (Just (EveryMonthsOnDay months day todm))

    it "works for this example of multiple months" $
      mkRecurrence
        RecurrenceData
          { recurrenceDataOption = Months,
            recurrenceDataDays = Nothing,
            recurrenceDataDayTimeOfDay = Nothing,
            recurrenceDataMonths = Just 8,
            recurrenceDataMonthDay = Nothing,
            recurrenceDataMonthTimeOfDay = Nothing
          }
        `shouldBe` Just (Just (EveryMonthsOnDay 8 Nothing Nothing))

  describe "Add" $ do
    let maxItems = 5
    paidTicklerWebServerSpec maxItems $ do
      it "gets an error when adding more than the maximum number of items" $ \yc ->
        forAll (replicateM maxItems genValid) $ \items ->
          forAllValid $ \extraItem ->
            runYesodClientM yc $ do
              withExampleAccountAndLogin_ $ do
                get AddR
                statusIs 200
                mapM_ addItem items
                addItem extraItem
                statusIs 200
                bodyContains "limit"

    freeTicklerWebServerSpec $ do
      it "gets a 200 even when adding 10 items" $ \yc -> do
        forAll (replicateM maxItems genValid) $ \items ->
          runYesodClientM yc $
            withExampleAccountAndLogin_ $ do
              get AddR
              statusIs 200
              mapM_ addItem items
              statusIs 200

    ticklerWebServerSpec $ do
      it "gets a 200 for a logged-in user" $ do
        withExampleAccountAndLogin_ $ do
          get AddR
          statusIs 200

      it "can post an example item without recurrence" $
        withExampleAccountAndLogin_ $ do
          addItem $
            Tickle
              { tickleContent = "hello",
                tickleScheduledDay = fromGregorian 2200 12 03,
                tickleScheduledTime = Nothing,
                tickleRecurrence = Nothing
              }
          statusIs 200

      it "can post an example item with daily recurrence" $
        withExampleAccountAndLogin_ $ do
          addItem $
            Tickle
              { tickleContent = "hello world",
                tickleScheduledDay = fromGregorian 2200 12 04,
                tickleScheduledTime = Nothing,
                tickleRecurrence =
                  Just $
                    EveryDaysAtTime 5 (Just (TimeOfDay 12 34 00))
              }
          statusIs 200

      it "can post an example item with monthly recurrence" $
        withExampleAccountAndLogin_ $ do
          addItem $
            Tickle
              { tickleContent = "hello world",
                tickleScheduledDay = fromGregorian 2200 12 04,
                tickleScheduledTime = Nothing,
                tickleRecurrence =
                  Just $
                    EveryMonthsOnDay 6 (Just 15) (Just (TimeOfDay 23 45 00))
              }
          statusIs 200

      it "can post any item" $ \yc ->
        forAllValid $ \item -> runYesodClientM yc $
          withExampleAccountAndLogin_ $ do
            addItem item
            statusIs 200
