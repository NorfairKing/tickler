module Tickler.Web.Server.Handler.TriggersSpec where

import TestImport

import Yesod.Test

import Tickler.Web.Server.Foundation
import Tickler.Web.Server.TestUtils

spec :: Spec
spec =
    ticklerWebServerSpec $
    ydescribe "Triggers" $
    yit "gets a 200 for a logged-in user" $ do
        withExampleAccountAndLogin_ $ do
            get TriggersR
            statusIs 200
