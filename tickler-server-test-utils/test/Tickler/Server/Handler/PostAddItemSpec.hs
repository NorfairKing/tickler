{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module Tickler.Server.Handler.PostAddItemSpec
  ( spec
  ) where

import TestImport

import Tickler.Client

import Tickler.API.Gen ()
import Tickler.Server.TestUtils

spec :: Spec
spec =
  withTicklerServer $
  describe "PostAddItem" $
  it "adds an item without crashing" $ \cenv ->
    forAllValid $ \t ->
      withValidNewUser cenv $ \token -> do
        uuid <- runClientOrError cenv $ clientPostAddItem token t
        shouldBeValid uuid
