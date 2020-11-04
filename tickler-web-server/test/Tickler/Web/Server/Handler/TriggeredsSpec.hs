module Tickler.Web.Server.Handler.TriggeredsSpec where

import TestImport
import Tickler.Web.Server.Foundation
import Tickler.Web.Server.TestUtils
import Yesod.Test

spec :: Spec
spec =
  ticklerWebServerSpec
    $ ydescribe "Triggereds"
    $ yit "gets a 200 for a logged-in user"
    $ withExampleAccountAndLogin_
    $ do
      get TriggeredsR
      statusIs 200
