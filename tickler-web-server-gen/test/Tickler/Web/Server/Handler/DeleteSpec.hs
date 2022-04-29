{-# LANGUAGE OverloadedStrings #-}

module Tickler.Web.Server.Handler.DeleteSpec where

import Test.Syd.Yesod
import TestImport
import Tickler.Web.Server.Foundation
import Tickler.Web.Server.TestUtils

spec :: Spec
spec =
  ticklerWebServerSpec $ do
    it "can delete an item that was just added" $ \yc ->
      forAllValid $ \tickle ->
        runYesodClientM yc $
          withExampleAccountAndLogin_ $ do
            uuid <- addItem tickle
            request $ do
              setUrl $ DeleteR uuid
              setMethod methodPost
              addToken
            statusIs 303
            _ <- followRedirect
            statusIs 200
            get $ EditR uuid
            statusIs 404
