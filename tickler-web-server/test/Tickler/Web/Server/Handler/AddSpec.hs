{-# LANGUAGE OverloadedStrings #-}

module Tickler.Web.Server.Handler.AddSpec where

import Network.HTTP.Types
import Test.Syd.Yesod
import TestImport
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
      it "gets an error when adding more than the maximum number of items" $ do
        withExampleAccountAndLogin_ $ do
          get AddR
          statusIs 200
          let addAnItem = do
                request $ do
                  setMethod methodPost
                  setUrl AddR
                  addTokenFromCookie
                  addPostParam "contents" "hello"
                  addPostParam "scheduled-day" "2200-12-03"
                  addPostParam "scheduled-time" ""
                  addPostParam "recurrence" "NoRecurrence"
                  addPostParam "days" ""
                  addPostParam "day-time-of-day" ""
                  addPostParam "months" ""
                  addPostParam "day" ""
                  addPostParam "month-time-of-day" ""
          replicateM_ maxItems $ do
            addAnItem
            statusIs 303
            locationShouldBe AddR
            _ <- followRedirect
            statusIs 200
          addAnItem
          statusIs 303
          locationShouldBe AddR
          _ <- followRedirect
          statusIs 200
          bodyContains "limit"

    freeTicklerWebServerSpec $ do
      it "gets a 200 even when adding 10 items" $ do
        withExampleAccountAndLogin_ $ do
          get AddR
          statusIs 200
          replicateM_ 10 $ do
            request $ do
              setMethod methodPost
              setUrl AddR
              addTokenFromCookie
              addPostParam "contents" "hello"
              addPostParam "scheduled-day" "2200-12-03"
              addPostParam "scheduled-time" ""
              addPostParam "recurrence" "NoRecurrence"
              addPostParam "days" ""
              addPostParam "day-time-of-day" ""
              addPostParam "months" ""
              addPostParam "day" ""
              addPostParam "month-time-of-day" ""
            statusIs 303
            locationShouldBe AddR
            _ <- followRedirect
            statusIs 200

    ticklerWebServerSpec $ do
      it "gets a 200 for a logged-in user" $ do
        withExampleAccountAndLogin_ $ do
          get AddR
          statusIs 200

      it "can post an example item without recurrence" $
        withExampleAccountAndLogin_ $ do
          get AddR
          statusIs 200
          request $ do
            setMethod methodPost
            setUrl AddR
            addTokenFromCookie
            addPostParam "contents" "hello"
            addPostParam "scheduled-day" "2200-12-03"
            addPostParam "scheduled-time" ""
            addPostParam "recurrence" "NoRecurrence"
            addPostParam "days" ""
            addPostParam "day-time-of-day" ""
            addPostParam "months" ""
            addPostParam "day" ""
            addPostParam "month-time-of-day" ""
          statusIs 303
          locationShouldBe AddR
          _ <- followRedirect
          statusIs 200

      it "can post an example item with daily" $
        withExampleAccountAndLogin_ $ do
          get AddR
          statusIs 200
          request $ do
            setMethod methodPost
            setUrl AddR
            addTokenFromCookie
            addPostParam "contents" "hello"
            addPostParam "scheduled-day" "2200-12-03"
            addPostParam "scheduled-time" ""
            addPostParam "recurrence" "Days"
            addPostParam "days" "5"
            addPostParam "day-time-of-day" "12:34"
            addPostParam "months" ""
            addPostParam "day" ""
            addPostParam "month-time-of-day" ""
          statusIs 303
          locationShouldBe AddR
          _ <- followRedirect
          statusIs 200

      it "can post an example item with monthly recurrence" $
        withExampleAccountAndLogin_ $ do
          get AddR
          statusIs 200
          request $ do
            setMethod methodPost
            setUrl AddR
            addTokenFromCookie
            addPostParam "contents" "hello"
            addPostParam "scheduled-day" "2200-12-03"
            addPostParam "scheduled-time" ""
            addPostParam "recurrence" "Months"
            addPostParam "days" ""
            addPostParam "day-time-of-day" ""
            addPostParam "months" "6"
            addPostParam "day" "15"
            addPostParam "month-time-of-day" "08:03"
          statusIs 303
          locationShouldBe AddR
          _ <- followRedirect
          statusIs 200
