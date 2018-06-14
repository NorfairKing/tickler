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
                    addPostParam "date" "2200-12-03"
                    addPostParam "time" ""
                statusIs 200
