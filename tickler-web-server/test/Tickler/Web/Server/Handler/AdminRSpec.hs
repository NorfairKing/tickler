{-# LANGUAGE OverloadedStrings #-}

module Tickler.Web.Server.Handler.AdminRSpec where

import TestImport
import Tickler.Web.Server.Foundation
import Tickler.Web.Server.TestUtils
import Yesod.Test

{-# ANN module ("HLint: ignore Reduce duplication" :: String) #-}

spec :: Spec
spec =
  ticklerWebServerSpec
    $ ydescribe "AdminR"
    $ do
      yit "gets a 200 when logged in as admin"
        $ withAdminAccount_
        $ do
          get AdminR
          statusIs 200
      yit "gets a 404 when not logged in as admin"
        $ withExampleAccount_
        $ do
          get AdminR
          statusIs 404
