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
    it "gets a 200 for a logged-in user's item" $ \yc ->
      forAllValid $ \item ->
        runYesodClientM yc $
          withExampleAccountAndLogin_ $ do
            uuid <- addItem item
            get $ EditR uuid
            statusIs 200

    it "gets a 404 for a another user's item" $ \yc ->
      forAllValid $ \un1 ->
        forAllValid $ \un2 ->
          forAllValid $ \item ->
            runYesodClientM yc $ do
              uuid <- withFreshAccount un1 "pw1" $ do
                addItem item
              withFreshAccount un2 "pw2" $ do
                get $ EditR uuid
                statusIs 404

    it "can edit an item" $ \yc ->
      forAllValid $ \initialItem ->
        forAllValid $ \editedItem ->
          runYesodClientM yc $
            withExampleAccountAndLogin_ $ do
              uuid <- addItem initialItem
              editItem uuid editedItem

    it "connot edit another user's item" $ \yc ->
      forAllValid $ \un1 ->
        forAllValid $ \un2 ->
          forAllValid $ \initialItem ->
            forAllValid $ \editedItem ->
              runYesodClientM yc $ do
                uuid <- withFreshAccount un1 "pw1" $ do
                  addItem initialItem
                withFreshAccount un2 "pw2" $ do
                  request $ editItemRequestBuilder uuid editedItem
                  statusIs 404
