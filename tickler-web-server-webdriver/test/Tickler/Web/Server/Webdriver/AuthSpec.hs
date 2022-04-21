{-# LANGUAGE OverloadedStrings #-}

module Tickler.Web.Server.Webdriver.AuthSpec (spec) where

import Tickler.Web.Server.Webdriver.TestImport

spec :: WebdriverSpec App
spec = do
  it "can register a dummy user" $ do
    openRoute HomeR
    driveRegister dummyUser

  it "can logout a dummy user" $ do
    openRoute HomeR
    driveRegister dummyUser
    driveLogout

  it "can login a dummy user" $ do
    openRoute HomeR
    driveRegister dummyUser
    driveLogout
    driveLogin dummyUser
