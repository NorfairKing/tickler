module Tickler.Web.Server.TestUtils.Webdriver where

import Tickler.Web.Server.Foundation
import Tickler.Web.Server.TestUtils

ticklerWebdriverSpec :: WebdriverSpec App -> Spec
ticklerWebdriverSpec = webdriverYesodSpec ticklerWebServerSpec
