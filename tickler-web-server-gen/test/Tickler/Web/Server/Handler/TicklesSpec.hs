{-# LANGUAGE OverloadedStrings #-}

module Tickler.Web.Server.Handler.TicklesSpec where

import Test.Syd.Yesod
import TestImport
import Tickler.Client
import Tickler.Web.Server.Foundation
import Tickler.Web.Server.TestUtils

spec :: Spec
spec = do
  ticklerWebServerSpec $
    describe "Tickles" $ do
      it "gets a 200 for a logged-in user" $
        withExampleAccountAndLogin_ $ do
          get TicklesR
          statusIs 200

  freeTicklerWebServerSpec $
    it "gets a 200 for a logged-in user when there are tickles" $ \yc ->
      forAllValid $ \items ->
        runYesodClientM yc $
          withExampleAccountAndLogin_ $ do
            get AddR
            statusIs 200
            mapM_ addItem (items :: [Tickle])
            get TicklesR
            statusIs 200
