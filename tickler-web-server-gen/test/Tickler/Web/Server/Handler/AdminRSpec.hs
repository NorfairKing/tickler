{-# LANGUAGE OverloadedStrings #-}

module Tickler.Web.Server.Handler.AdminRSpec where

import Test.Syd.Yesod
import TestImport
import Tickler.Web.Server.Foundation
import Tickler.Web.Server.TestUtils

spec :: Spec
spec =
  ticklerWebServerSpec $ do
    describe "AdminR" $ do
      it "gets a 200 when logged in as admin" $
        withAdminAccount_ $ do
          get $ AdminR AdminPanelR
          statusIs 200
      it "gets a 404 when not logged in as admin" $
        withExampleAccount_ $ do
          get $ AdminR AdminPanelR
          statusIs 404
