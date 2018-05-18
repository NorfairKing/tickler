{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module Tickler.Server.Handler.AdminGetAccountsSpec
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
    describe "AdminGetAccounts" $ do
        it "forbids non-admin users from getting account info" $ \cenv ->
            requiresAdmin cenv clientAdminGetAccounts
        it "only returns valid account info" $ \cenv ->
            withAdmin cenv $ \token -> do
                accountInfos <-
                    runClientOrError cenv $ clientAdminGetAccounts token
                shouldBeValid accountInfos
