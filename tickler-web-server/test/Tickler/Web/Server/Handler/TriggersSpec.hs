module Tickler.Web.Server.Handler.TriggersSpec where

import TestImport
import Tickler.Web.Server.Foundation
import Tickler.Web.Server.TestUtils
import Yesod.Test

spec :: Spec
spec =
  ticklerWebServerSpec
    $ ydescribe "Triggers"
    $ yit "gets a 200 for a logged-in user"
    $ withExampleAccountAndLogin_
    $ do
      get TriggersR
      statusIs 200
