{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module Tickler.Server.Handler.SyncSpec
    ( spec
    ) where

import TestImport

import Tickler.Client
import Tickler.Client.Store

import Tickler.Client.Gen ()
import Tickler.Data.Gen ()
import Tickler.Server.TestUtils

spec :: Spec
spec =
    withTicklerServer $
    describe "PostSync" $ do
        it "produces a valid sync result for any sync request" $ \cenv ->
            forAllValid $ \syncRequest ->
                withValidNewUser cenv $ \token -> do
                    sr <-
                        runClientOrError cenv $ clientPostSync token syncRequest
                    shouldBeValid sr
        it "is idempotent" $ \cenv ->
            forAllValid $ \initStore ->
                withValidNewUser cenv $ \token -> do
                    sr1 <-
                        runClientOrError cenv $
                        clientPostSync token $ makeSyncRequest initStore
                    let firstStore = mergeStore initStore sr1
                    sr2 <-
                        runClientOrError cenv $
                        clientPostSync token $ makeSyncRequest firstStore
                    let secondStore = mergeStore firstStore sr2
                    secondStore `shouldBe` firstStore
