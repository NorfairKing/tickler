{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Tickler.Web.Server.Webdriver.TriggerAddEmail.TestUtils where

import Data.Text (Text)
import Test.Syd
import Test.Syd.Webdriver
import Test.WebDriver
import Tickler.API
import Tickler.Client
import Tickler.Web.Server.Foundation
import Tickler.Web.Server.Webdriver.Auth.TestUtils
import Tickler.Web.Server.Webdriver.TestUtils

driveTriggerAddEmail :: TestUser -> Text -> WebdriverTestM App EmailTriggerInfo
driveTriggerAddEmail user ea = do
  findElem (ById "nav-triggers") >>= click
  findElem (ByName "email-address") >>= sendKeys ea
  findElem (ById "submit-email") >>= submit
  token <- loginViaAPI user
  triggers <- driveClientOrErr $ clientGetTriggers token
  liftIO $ case triggers of
    [] -> expectationFailure "Got no triggers."
    [TriggerInfo {..}] -> case triggerInfo of
      TriggerEmail eti -> pure eti
      _ -> expectationFailure "Expected an email trigger."
    _ -> expectationFailure "Got more than one one trigger."
