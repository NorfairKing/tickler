{-# LANGUAGE OverloadedStrings #-}

module Tickler.Web.Server.Handler.TicklesSpec where

import Test.Syd.Yesod
import TestImport
import Tickler.Web.Server.Foundation
import Tickler.Web.Server.TestUtils

spec :: Spec
spec =
  ticklerWebServerSpec $
    describe "Tickles" $ do
      it "gets a 200 for a logged-in user" $
        withExampleAccountAndLogin_ $ do
          get TicklesR
          statusIs 200
      it "gets a 200 for a logged-in user when there is a tickle" $
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
