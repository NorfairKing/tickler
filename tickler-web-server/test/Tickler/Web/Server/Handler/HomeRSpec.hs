module Tickler.Web.Server.Handler.HomeRSpec where

import Test.Syd.Yesod
import TestImport
import Tickler.Web.Server.Foundation
import Tickler.Web.Server.TestUtils

spec :: Spec
spec =
  ticklerWebServerSpec $
    ydescribe "HomeR" $
      do
        yit "gets a 200 for non-logged-in user" $ do
          get HomeR
          statusIs 200
        yit "gets a 200 for an example user" $
          withExampleAccount_ $
            do
              get HomeR
              statusIs 200
