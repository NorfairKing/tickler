module Tickler.Web.Server.Webdriver.HomeSpec (spec) where

import Tickler.Web.Server.Webdriver.TestImport

spec :: WebdriverSpec App
spec =
  it "can open HomeR" $ do
    openRoute HomeR
