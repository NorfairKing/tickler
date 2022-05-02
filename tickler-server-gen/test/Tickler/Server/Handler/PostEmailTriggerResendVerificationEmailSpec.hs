module Tickler.Server.Handler.PostEmailTriggerResendVerificationEmailSpec (spec) where

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
    it "cannot ask to resend an email for a trigger that does not exist" $ \cenv ->
      forAllValid $ \uuid ->
        withValidNewUser cenv $ \token -> do
          errOrResult <- runClient cenv $ clientPostEmailTriggerResendVerificationEmail token uuid
          case errOrResult of
            Left err ->
              case err of
                FailureResponse _ resp -> responseStatusCode resp `shouldBe` notFound404
                _ -> expectationFailure $ unwords ["Unexpected error:", show err]
            Right _ -> expectationFailure "Should not have succeeded."

    it "fails to ask to resend a verification email if one is already queued" $ \cenv ->
      forAllValid $ \addEmailTrigger ->
        withValidNewUser cenv $ \token -> do
          uuid <- runClientOrError cenv $ clientPostEmailTrigger token addEmailTrigger
          errOrResult <- runClient cenv $ clientPostEmailTriggerResendVerificationEmail token uuid
          case errOrResult of
            Left err ->
              case err of
                FailureResponse _ resp -> responseStatusCode resp `shouldBe` badRequest400
                _ -> expectationFailure $ unwords ["Unexpected error:", show err]
            Right _ -> expectationFailure "Should not have succeeded."

  withTicklerServerAndDatabase $ do
    it "can ask to resend a verification email" $ \(pool, cenv) ->
      forAllValid $ \addEmailTrigger ->
        withValidNewUser cenv $ \token -> do
          uuid <- runClientOrError cenv $ clientPostEmailTrigger token addEmailTrigger
          -- First delete any emails that we've already scheduled.
          runPersistentTest pool $ DB.deleteWhere [VerificationEmailTrigger ==. uuid]
          NoContent <- runClientOrError cenv $ clientPostEmailTriggerResendVerificationEmail token uuid
          verificationEmails <- runPersistentTest pool $ DB.selectList [VerificationEmailTrigger ==. uuid] []
          case verificationEmails of
            [] -> expectationFailure "Expected to find one verification emails, found none."
            [_] -> pure ()
            _ -> expectationFailure "Expected to find one verification emails, found more."

    it "fails to ask to resend a verification email if the trigger has already been verified" $ \(pool, cenv) ->
      forAllValid $ \addEmailTrigger ->
        withValidNewUser cenv $ \token -> do
          uuid <- runClientOrError cenv $ clientPostEmailTrigger token addEmailTrigger
          runPersistentTest pool $ DB.updateWhere [EmailTriggerIdentifier ==. uuid] [EmailTriggerVerified =. True]
          errOrResult <- runClient cenv $ clientPostEmailTriggerResendVerificationEmail token uuid
          case errOrResult of
            Left err ->
              case err of
                FailureResponse _ resp -> responseStatusCode resp `shouldBe` badRequest400
                _ -> expectationFailure $ unwords ["Unexpected error:", show err]
            Right _ -> expectationFailure "Should not have succeeded."
