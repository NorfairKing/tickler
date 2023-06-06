module Tickler.Web.Server.Handler.TriggerAddEmailSpec where

import TestImport
import Tickler.Web.Server.TestUtils

spec :: Spec
spec =
  ticklerWebServerSpec $ do
    it "can post an email trigger" $ \yc -> do
      forAllValid $ \emailTrigger ->
        runYesodClientM yc $
          withExampleAccountAndLogin_ $
            addEmailTrigger emailTrigger
