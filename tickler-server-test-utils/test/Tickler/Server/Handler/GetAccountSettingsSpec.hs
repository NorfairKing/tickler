{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module Tickler.Server.Handler.GetAccountSettingsSpec
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
    describe "GetAccountSettings" $
    it "returns valid account settings" $ \cenv ->
        withValidNewUser cenv $ \token -> do
            accountInfo <-
                runClientOrError cenv $ clientGetAccountSettings token
            shouldBeValid accountInfo
