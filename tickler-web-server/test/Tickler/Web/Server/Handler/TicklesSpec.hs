module Tickler.Web.Server.Handler.TicklesSpec where

import TestImport
import Tickler.Web.Server.Foundation
import Tickler.Web.Server.TestUtils
import Yesod.Test

spec :: Spec
spec =
  ticklerWebServerSpec
    $ ydescribe "Tickles"
    $ yit "gets a 200 for a logged-in user"
    $ withExampleAccountAndLogin_
    $ do
      get TicklesR
      statusIs 200
