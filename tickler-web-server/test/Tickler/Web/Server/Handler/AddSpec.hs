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
      forAllValid $
        \(days, todd) ->
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
      forAllValid $
        \(months, day, todm) ->
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
  ticklerWebServerSpec $
    ydescribe "Add" $
      --  yit "gets a 200 for a logged-in user" $ do
      --      withExampleAccountAndLogin_ $ do
      --          get AddR
      --          statusIs 200
      yit "can post an example item" $
        withExampleAccountAndLogin_ $
          do
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
            loc1 <- getLocation
            void followRedirect
            liftIO $ loc1 `shouldBe` Right AddR
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
            loc2 <- getLocation
            void followRedirect
            liftIO $ loc2 `shouldBe` Right AddR
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
            loc <- getLocation
            void followRedirect
            liftIO $ loc `shouldBe` Right AddR
            statusIs 200
