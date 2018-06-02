module Tickler.Web.Server.Handler.TriggeredSpec where

import TestImport

import Yesod.Test

import Tickler.Web.Server.Foundation
import Tickler.Web.Server.TestUtils

spec :: Spec
spec =
    ticklerWebServerSpec $
    ydescribe "Triggered" $
    yit "gets a 200 for a logged-in user" $ do
        withExampleAccountAndLogin_ $ do
            get TriggeredR
            statusIs 200
