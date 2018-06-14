{-# LANGUAGE OverloadedStrings #-}

module Tickler.Web.Server.Handler.AddSpec where

import TestImport

import Yesod.Test

import Network.HTTP.Types

import Tickler.Web.Server.Foundation
import Tickler.Web.Server.TestUtils

spec :: Spec
spec =
    ticklerWebServerSpec $
    ydescribe "Add" $
       --  yit "gets a 200 for a logged-in user" $ do
       --      withExampleAccountAndLogin_ $ do
       --          get AddR
       --          statusIs 200
    yit "can post an example item" $ do
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
            loc <- getLocation
            void followRedirect
            liftIO $ loc `shouldBe` Right AddR
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
            loc <- getLocation
            void followRedirect
            liftIO $ loc `shouldBe` Right AddR
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
