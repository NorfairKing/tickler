{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Tickler.Server.Handler.PostEmailTriggerVerifySpec (spec) where

import Database.Persist as DB
import Test.Syd.Persistent
import TestImport
import Tickler.API
import Tickler.API.Gen ()
import Tickler.Client
import Tickler.Server.TestUtils

spec :: Spec
spec = do
  withTicklerServer $ do
    it "cannot verify a trigger that does not exist" $ \cenv ->
      forAllValid $ \uuid ->
        forAllValid $ \verificationKey ->
          withValidNewUser cenv $ \token -> do
            errOrResult <- runClient cenv $ clientPostEmailTriggerVerify token uuid verificationKey
            case errOrResult of
              Left err ->
                case err of
                  FailureResponse _ resp -> responseStatusCode resp `shouldBe` notFound404
                  _ -> expectationFailure $ unwords ["Unexpected error:", show err]
              Right _ -> expectationFailure "Should not have succeeded."

    it "fails to verify an email trigger with the wrong verification key" $ \cenv ->
      forAllValid $ \addEmailTrigger ->
        withValidNewUser cenv $ \token -> do
          otherVerificationKey <- generateRandomVerificationKey
          uuid <- runClientOrError cenv $ clientPostEmailTrigger token addEmailTrigger
          errOrResult <- runClient cenv $ clientPostEmailTriggerVerify token uuid otherVerificationKey
          case errOrResult of
            Left err ->
              case err of
                FailureResponse _ resp -> responseStatusCode resp `shouldBe` badRequest400
                _ -> expectationFailure $ unwords ["Unexpected error:", show err]
            Right _ -> expectationFailure "Should not have succeeded."
          TriggerInfo {..} <- runClientOrError cenv $ clientGetTrigger token uuid
          liftIO $ case triggerInfo of
            TriggerIntray _ -> expectationFailure "should have been an email trigger."
            TriggerEmail EmailTriggerInfo {..} -> emailTriggerInfoVerified `shouldBe` False

  withTicklerServerAndDatabase $ do
    it "verifies an email trigger that was just added" $ \(pool, cenv) ->
      forAllValid $ \addEmailTrigger ->
        withValidNewUser cenv $ \token -> do
          uuid <- runClientOrError cenv $ clientPostEmailTrigger token addEmailTrigger
          mVerificationKey <- runPersistentTest pool $ fmap (emailTriggerVerificationKey . entityVal) <$> DB.getBy (UniqueEmailTrigger uuid)
          case mVerificationKey of
            Nothing -> expectationFailure "Expected to find an email trigger"
            Just verificationKey -> do
              TriggerInfo {..} <- runClientOrError cenv $ do
                NoContent <- clientPostEmailTriggerVerify token uuid verificationKey
                clientGetTrigger token uuid
              liftIO $ case triggerInfo of
                TriggerIntray _ -> expectationFailure "should have been an email trigger."
                TriggerEmail EmailTriggerInfo {..} -> emailTriggerInfoVerified `shouldBe` True
