{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module Tickler.Server.Handler.ListItemUUIDsSpec
  ( spec
  ) where

import TestImport

import Tickler.Client

import Tickler.API.Gen ()
import Tickler.Server.TestUtils

spec :: Spec
spec =
  withTicklerServerFree $
  describe "ListItemUUIDs" $ do
    it "it lists item uuids of items that were just added" $ \cenv ->
      forAllValid $ \items ->
        withValidNewUser cenv $ \token -> do
          uuids <- runClientOrError cenv $ mapM (clientPostAddItem token) items
          itemUUIDs' <- runClientOrError cenv $ clientGetItemUUIDs token
          itemUUIDs' `shouldContain` uuids
    it "it always lists valid item uuids" $ \cenv ->
      forAllValid $ \items ->
        withValidNewUser cenv $ \token -> do
          void $ runClientOrError cenv $ mapM (clientPostAddItem token) (items :: [AddItem])
          itemUUIDs <- runClientOrError cenv $ clientGetItemUUIDs token
          shouldBeValid itemUUIDs
    it "does not list others' item uuids" $ \cenv ->
      forAllValid $ \items1 ->
        forAllValid $ \items2 ->
          withValidNewUser cenv $ \token1 ->
            withValidNewUser cenv $ \token2 -> do
              uuids1 <- runClientOrError cenv $ mapM (clientPostAddItem token1) items1
              uuids2 <- runClientOrError cenv $ mapM (clientPostAddItem token2) items2
              itemUUIDs' <- runClientOrError cenv $ clientGetItemUUIDs token1
              itemUUIDs' `shouldContain` uuids1
              forM_ (uuids2 :: [ItemUUID]) $ \u -> u `shouldNotSatisfy` (`elem` itemUUIDs')
