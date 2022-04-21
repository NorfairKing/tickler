{-# LANGUAGE OverloadedStrings #-}

module Tickler.Web.Server.Webdriver.Auth.TestUtils where

import Data.Text (Text)
import Test.Syd.Webdriver
import Test.WebDriver
import Tickler.API
import Tickler.Web.Server.Foundation

driveRegister :: Username -> Text -> WebdriverTestM App ()
driveRegister username password = do
  findElem (ById "nav-register") >>= click
  findElem (ByName "username") >>= sendKeys (usernameText username)
  findElem (ByName "passphrase") >>= sendKeys password
  findElem (ByName "passphrase-confirm") >>= sendKeys password
  findElem (ById "submit") >>= submit
