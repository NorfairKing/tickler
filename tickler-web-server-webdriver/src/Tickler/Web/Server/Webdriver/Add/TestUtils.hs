{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Tickler.Web.Server.Webdriver.Add.TestUtils where

import Test.Syd
import Test.Syd.Webdriver
import Test.Syd.Webdriver.Yesod
import Test.WebDriver
import Tickler.API
import Tickler.Web.Server.Foundation

driveAddTickle :: Tickle -> WebdriverTestM App ItemUUID
driveAddTickle Tickle {..} = do
  findElem (ById "nav-add") >>= click
  findElem (ByName "contents") >>= sendKeys tickleContent
  findElem (ById "submit") >>= submit
  route <- getCurrentRoute
  liftIO $ case route of
    EditR uuid -> pure uuid
    _ -> expectationFailure "Expected to be on an EditR"
