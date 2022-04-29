{-# LANGUAGE OverloadedStrings #-}

module Tickler.Web.Server.Webdriver.Delete.TestUtils where

import Test.Syd.Webdriver
import Test.Syd.Webdriver.Yesod
import Test.WebDriver
import Test.WebDriver.Commands.Wait
import Tickler.API
import Tickler.Web.Server.Foundation

driveDeleteTickle :: ItemUUID -> WebdriverTestM App ()
driveDeleteTickle uuid = do
  findElem (ById "nav-tickles") >>= click
  findElem (ById $ uuidText uuid) >>= \e -> findElemFrom e (ById "delete") >>= click
  acceptAlert
  waitUntil 5 $ do
    route <- getCurrentRoute
    expect $ route == TicklesR
