{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Tickler.Web.Server.Webdriver.Auth.TestUtils where

import Data.Maybe
import Data.Text (Text)
import Test.Syd.Webdriver
import Test.WebDriver
import Tickler.API
import Tickler.Web.Server.Foundation

data TestUser = TestUser
  { testUserUsername :: !Username,
    testUserPassword :: !Text
  }
  deriving (Show, Eq)

dummyUser :: TestUser
dummyUser =
  TestUser
    { testUserUsername = fromJust $ parseUsername "dummy",
      testUserPassword = "password"
    }

dummyUser2 :: TestUser
dummyUser2 =
  TestUser
    { testUserUsername = fromJust $ parseUsername "dummy2",
      testUserPassword = "password2"
    }

driveRegister :: TestUser -> WebdriverTestM App ()
driveRegister TestUser {..} = do
  findElem (ById "nav-register") >>= click
  findElem (ByName "username") >>= sendKeys (usernameText testUserUsername)
  findElem (ByName "passphrase") >>= sendKeys testUserPassword
  findElem (ByName "passphrase-confirm") >>= sendKeys testUserPassword
  findElem (ById "submit") >>= submit

driveLogin :: TestUser -> WebdriverTestM App ()
driveLogin TestUser {..} = do
  findElem (ById "nav-login") >>= click
  findElem (ByName "username") >>= sendKeys (usernameText testUserUsername)
  findElem (ByName "passphrase") >>= sendKeys testUserPassword
  findElem (ById "submit") >>= submit

driveLogout :: WebdriverTestM App ()
driveLogout =
  findElem (ById "nav-logout") >>= click

driveAsNewUser :: TestUser -> WebdriverTestM App a -> WebdriverTestM App a
driveAsNewUser testUser func = do
  driveRegister testUser
  result <- func
  driveLogout
  pure result

driveAsUser :: TestUser -> WebdriverTestM App a -> WebdriverTestM App a
driveAsUser testUser func = do
  driveLogin testUser
  result <- func
  driveLogout
  pure result
