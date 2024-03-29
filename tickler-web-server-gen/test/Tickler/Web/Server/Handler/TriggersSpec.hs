module Tickler.Web.Server.Handler.TriggersSpec where

import Test.Syd.Yesod
import TestImport
import Tickler.Web.Server.Foundation
import Tickler.Web.Server.TestUtils

spec :: Spec
spec = ticklerWebServerSpec $
  describe "Triggers" $
    it "gets a 200 for a logged-in user" $
      withExampleAccountAndLogin_ $ do
        get TriggersR
        statusIs 200
