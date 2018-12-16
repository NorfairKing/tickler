module Tickler.Web.Server.Handler.TriggeredsSpec where

import TestImport

import Yesod.Test

import Tickler.Web.Server.Foundation
import Tickler.Web.Server.TestUtils

spec :: Spec
spec =
    ticklerWebServerSpec $
    ydescribe "Triggereds" $
    yit "gets a 200 for a logged-in user" $ do
        withExampleAccountAndLogin_ $ do
            get TriggeredsR
            statusIs 200
