{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module Tickler.Server.Handler.GetItemSpec
  ( spec
  ) where

import TestImport

import Tickler.API
import Tickler.Client

import Tickler.API.Gen ()
import Tickler.Server.TestUtils

spec :: Spec
spec =
  withTicklerServer $
  describe "GetItem" $
  it "gets the same item that was just added" $ \cenv ->
    forAllValid $ \ti ->
      withValidNewUser cenv $ \token -> do
        i <-
          runClientOrError cenv $ do
            uuid <- clientPostAddItem token ti
            clientGetItem token uuid
        itemInfoContents i `shouldBe` ti
