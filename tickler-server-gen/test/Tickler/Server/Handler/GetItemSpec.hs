{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Tickler.Server.Handler.GetItemSpec
  ( spec,
  )
where

import TestImport
import Tickler.API
import Tickler.API.Gen ()
import Tickler.Client
import Tickler.Server.TestUtils

spec :: Spec
spec =
  withTicklerServer $ do
    it "gets the same item that was just added" $ \cenv ->
      forAllValid $ \ti ->
        withValidNewUser cenv $ \token -> do
          i <- runClientOrError cenv $ do
            uuid <- clientPostAddItem token ti
            clientGetItem token uuid
          itemInfoContents i `shouldBe` ti
    pending "fails to get another user's item"
