{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Tickler.Server.Handler.SyncSpec
  ( spec,
  )
where

import qualified Data.Map as M
import qualified Data.Mergeful as Mergeful
import TestImport
import Tickler.Client
import Tickler.Client.Gen ()
import Tickler.Server.TestUtils

spec :: Spec
spec =
  describe "PostSync" $ do
    withTicklerServer $ do
      it "produces a valid sync result for any sync request" $ \cenv ->
        forAllValid $ \syncRequest ->
          withValidNewUser cenv $ \token -> do
            sr <- runClientOrError cenv $ clientPostSync token syncRequest
            shouldBeValid sr
      it "is idempotent" $ \cenv ->
        forAllValid $ \initStore ->
          withValidNewUser cenv $ \token -> do
            sr1 <- runClientOrError cenv $ clientPostSync token $ makeSyncRequest initStore
            let firstStore = mergeSyncResponse initStore sr1
            sr2 <- runClientOrError cenv $ clientPostSync token $ makeSyncRequest firstStore
            let secondStore = mergeSyncResponse firstStore sr2
            secondStore `shouldBe` firstStore
    let maxFree = 2
    withPaidTicklerServer 2 $
      it "syncs at most two items if noly two items are free" $
        \cenv ->
          forAllValid $ \(i1, i2, i3) ->
            withValidNewUser cenv $ \token -> do
              let s@(Store store) =
                    addTickleToStore (addTickleToStore (addTickleToStore emptyStore i1) i2) i3
              M.size (Mergeful.clientStoreAddedItems store) `shouldBe` 3
              M.size (Mergeful.clientStoreSyncedItems store) `shouldBe` 0
              M.size (Mergeful.clientStoreSyncedButChangedItems store) `shouldBe` 0
              M.size (Mergeful.clientStoreDeletedItems store) `shouldBe` 0
              sr1 <- runClientOrError cenv $ clientPostSync token $ makeSyncRequest s
              let (Store store') = mergeSyncResponse s sr1
              M.size (Mergeful.clientStoreAddedItems store') `shouldBe` (3 - maxFree)
              M.size (Mergeful.clientStoreSyncedItems store') `shouldBe` maxFree
              M.size (Mergeful.clientStoreSyncedButChangedItems store) `shouldBe` 0
              M.size (Mergeful.clientStoreDeletedItems store) `shouldBe` 0
