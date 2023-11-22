{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Tickler.Web.Server.Webdriver.TriggerAddEmailSpec (spec) where

import Tickler.Web.Server.Webdriver.TestImport

spec :: WebdriverSpec App
spec =
  it "can add an email trigger" $ do
    driveAsNewUser dummyUser $ do
      let ea = "tickler@example.com"
      EmailTriggerInfo {..} <- driveTriggerAddEmail dummyUser ea
      liftIO $ emailAddressText emailTriggerInfoEmailAddress `shouldBe` ea
