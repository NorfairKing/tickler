{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Tickler.Web.Server.Webdriver.TriggerAddIntray.TestUtils where

import qualified Data.Text as T
import qualified Intray.Client as Intray
import Test.Syd
import Test.Syd.Webdriver
import Test.WebDriver
import Tickler.API
import Tickler.Client
import Tickler.Web.Server.Foundation
import Tickler.Web.Server.Webdriver.TestUtils

driveTriggerAddIntray :: Username -> Intray.Username -> BaseUrl -> Intray.AccessKeySecret -> WebdriverTestM App IntrayTriggerInfo
driveTriggerAddIntray ticklerUsername intrayUsername intrayBaseUrl intrayAccessKey = do
  findElem (ById "nav-triggers") >>= click
  -- Url
  urlE <- findElem (ByName "url")
  clearInput urlE
  sendKeys (T.pack (showBaseUrl intrayBaseUrl)) urlE
  -- Username
  usernameE <- findElem (ByName "username")
  clearInput usernameE
  sendKeys (Intray.usernameText intrayUsername) usernameE
  -- AccessKey
  findElem (ByName "access-key") >>= sendKeys (Intray.accessKeySecretText intrayAccessKey)
  findElem (ById "submit-intray") >>= submit
  -- Check that the trigger exists now.
  token <- getUserToken ticklerUsername
  triggers <- driveClientOrErr $ clientGetTriggers token
  liftIO $ case triggers of
    [] -> expectationFailure "Got no triggers."
    [TriggerInfo {..}] -> case triggerInfo of
      TriggerIntray iti -> pure iti
      _ -> expectationFailure "Expected an intray trigger."
    _ -> expectationFailure "Got more than one trigger."
