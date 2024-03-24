{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Tickler.Server.Handler.AdminGetAccountSpec
  ( spec,
  )
where

import TestImport
import Tickler.Client
import Tickler.Data.Gen ()
import Tickler.Server.TestUtils

spec :: Spec
spec = withTicklerServer
  $ describe "AdminGetAccount"
  $ do
    it "forbids non-admin users from getting account info" $ \cenv ->
      forAllValid $ \username ->
        requiresAdmin cenv $ \token ->
          clientAdminGetAccount token username

    it "gets a 404 when the user does not exist" $ \cenv ->
      forAllValid $ \username ->
        withAdmin cenv $ \adminToken -> do
          errOrAccountInfo <- runClient cenv $ clientAdminGetAccount adminToken username
          case errOrAccountInfo of
            Left err ->
              case err of
                FailureResponse _ resp -> responseStatusCode resp `shouldBe` notFound404
                _ -> expectationFailure $ unwords ["Unexpected error:", show err]
            Right _ -> expectationFailure "Should not have succeeded."

    it "returns the same account info as when a user logs in" $ \cenv ->
      withValidNewUserAndData cenv $ \_ _ userToken ->
        withAdmin cenv $ \adminToken ->
          runClientOrError cenv $ do
            accountInfoViaUser <- clientGetAccountInfo userToken
            accountInfoViaAdmin <- clientAdminGetAccount adminToken (accountInfoUsername accountInfoViaUser)
            liftIO $ accountInfoViaAdmin `shouldBe` accountInfoViaUser
