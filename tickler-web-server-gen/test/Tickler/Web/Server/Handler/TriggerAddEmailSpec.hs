{-# LANGUAGE OverloadedStrings #-}

module Tickler.Web.Server.Handler.TriggerAddEmailSpec where

import Test.Syd.Yesod
import TestImport
import Tickler.Client
import Tickler.Web.Server.Foundation
import Tickler.Web.Server.TestUtils

spec :: Spec
spec =
  ticklerWebServerSpec $ do
    it "can post an email trigger" $ \yc -> do
      forAllValid $ \emailAddress ->
        runYesodClientM yc $
          withExampleAccountAndLogin_ $ do
            get TriggersR
            statusIs 200
            request $ do
              setMethod methodPost
              setUrl TriggerAddIntrayR
              addToken
              addPostParam "email-address" $ emailAddressText emailAddress
            statusIs 303
            locationShouldBe TriggersR
            _ <- followRedirect
            statusIs 200
