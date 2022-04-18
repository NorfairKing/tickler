{-# LANGUAGE OverloadedStrings #-}

module Tickler.Web.Server.Handler.EditSpec where

import Network.HTTP.Types
import Test.Syd.Yesod
import TestImport
import Tickler.Data
import Tickler.Web.Server.Foundation
import Tickler.Web.Server.Handler.Item
import Tickler.Web.Server.TestUtils

spec :: Spec
spec =
  ticklerWebServerSpec $ do
    pending "gets a 200 for a logged-in user's item"
    pending "gets a 404 for a another user's item"
    it "can edit an item" $ \yc ->
      forAllValid $ \initialItem ->
        forAllValid $ \editedItem ->
          runYesodClientM yc $
            withExampleAccountAndLogin_ $ do
              uuid <- addItem initialItem
              editItem uuid editedItem
    pending "cannot edit another user's item"
