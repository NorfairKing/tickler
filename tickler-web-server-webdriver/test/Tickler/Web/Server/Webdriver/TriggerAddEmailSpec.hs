{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Tickler.Web.Server.Webdriver.TriggerAddEmailSpec (spec) where

import Tickler.Web.Server.Webdriver.TestImport

spec :: WebdriverSpec App
spec =
  it "can add an email trigger" $ do
    driveAsNewUser dummyUser $ do
      let emailAddress = "tickler@example.com"
      findElem (ById "nav-triggers") >>= click
      findElem (ByName "email-address") >>= sendKeys emailAddress
      findElem (ById "submit-email") >>= submit
      token <- getUserToken $ testUserUsername dummyUser
      triggers <- driveClientOrErr $ clientGetTriggers token
      liftIO $ case triggers of
        [] -> expectationFailure "Got no triggers."
        [TriggerInfo {..}] -> case triggerInfo of
          TriggerEmail EmailTriggerInfo {..} -> emailAddressText emailTriggerInfoEmailAddress `shouldBe` emailAddress
          _ -> expectationFailure "Expected an email trigger."
        _ -> expectationFailure "Got more than one one trigger."
