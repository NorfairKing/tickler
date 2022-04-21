{-# LANGUAGE OverloadedStrings #-}

module Tickler.Web.Server.Webdriver.AuthSpec (spec) where

import Tickler.Web.Server.Webdriver.TestImport

spec :: WebdriverSpec App
spec = do
  it "can register a dummy user" $ do
    openRoute HomeR
    username <- liftIO $ parseUsername "dummy"
    driveRegister username "password"
