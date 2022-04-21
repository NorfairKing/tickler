module Tickler.Web.Server.Webdriver.TestUtils where

import Database.Persist
import Test.Syd
import Test.Syd.Webdriver
import Test.Syd.Webdriver.Yesod
import Tickler.Server.TestUtils as API
import Tickler.Web.Server.Foundation
import Tickler.Web.Server.TestUtils

ticklerWebdriverSpec :: WebdriverSpec App -> Spec
ticklerWebdriverSpec = webdriverYesodSpec $ \man -> do
  -- Find a way to have both monetisation and without, if we can
  cenv <- API.ticklerTestClientEnvSetupFunc Nothing man
  appSetupFunc man cenv

driveDB :: SqlPersistT IO a -> WebdriverTestM App a
driveDB = undefined
