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
import Tickler.Web.Server.Webdriver.TestUtils

driveTriggerAddEmail :: Username -> Text -> WebdriverTestM App EmailTriggerInfo
driveTriggerAddEmail un ea = do
  findElem (ById "nav-triggers") >>= click
  findElem (ByName "email-address") >>= sendKeys ea
  findElem (ById "submit-email") >>= submit
  token <- getUserToken un
  triggers <- driveClientOrErr $ clientGetTriggers token
  liftIO $ case triggers of
    [] -> expectationFailure "Got no triggers."
    [TriggerInfo {..}] -> case triggerInfo of
      TriggerEmail eti -> pure eti
      _ -> expectationFailure "Expected an email trigger."
    _ -> expectationFailure "Got more than one one trigger."
