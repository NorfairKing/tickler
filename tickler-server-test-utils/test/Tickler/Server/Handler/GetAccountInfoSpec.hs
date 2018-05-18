{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module Tickler.Server.Handler.GetAccountInfoSpec
    ( spec
    ) where

import TestImport

import Tickler.Client

import Tickler.Client.Gen ()
import Tickler.Data.Gen ()
import Tickler.Server.TestUtils

spec :: Spec
spec =
    withTicklerServer $
    describe "GetAccountInfo" $ do
        it "returns valid account info" $ \cenv ->
            withValidNewUser cenv $ \token -> do
                accountInfo <-
                    runClientOrError cenv $ clientGetAccountInfo token
                shouldBeValid accountInfo
        it "gets account info with the right username" $ \cenv ->
            withValidNewUserAndData cenv $ \un _ token -> do
                accountInfo <-
                    runClientOrError cenv $ clientGetAccountInfo token
                accountInfoUsername accountInfo `shouldBe` un
