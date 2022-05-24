module Tickler.Server.Handler.AdminPutAccountSubscriptionSpec (spec) where

import TestImport
import Tickler.API.Gen ()
import Tickler.Client
import Tickler.Server.TestUtils

spec :: Spec
spec = do
  withTicklerServer $ do
    it "forbids non-admin users from deleting a user" $ \cenv ->
      forAllValid $ \username ->
        forAllValid $ \end ->
          requiresAdmin cenv $ \token ->
            clientAdminPutAccountSubscription token username end

    it "gets a 404 when the user does not exist" $ \cenv ->
      forAllValid $ \end ->
        forAllValid $ \username ->
          withAdmin cenv $ \adminToken -> do
            errOrAccountInfo <- runClient cenv $ clientAdminPutAccountSubscription adminToken username end
            case errOrAccountInfo of
              Left err ->
                case err of
                  FailureResponse _ resp -> responseStatusCode resp `shouldBe` notFound404
                  _ -> expectationFailure $ unwords ["Unexpected error:", show err]
              Right _ -> expectationFailure "Should not have succeeded."

  withFreeTicklerServer $
    it "sets the subscription time correctly" $ \cenv ->
      forAllValid $ \end ->
        withAdmin cenv $ \adminToken ->
          withValidNewUserAndData cenv $ \username _ userToken ->
            runClientOrError cenv $ do
              NoContent <- clientAdminPutAccountSubscription adminToken username end
              accountInfo <- clientGetAccountInfo userToken
              liftIO $ accountInfoStatus accountInfo `shouldBe` NoPaymentNecessary

  withPaidTicklerServer 5 $ do
    it "sets the subscription time correctly" $ \cenv ->
      forAllValid $ \end ->
        withAdmin cenv $ \adminToken ->
          withValidNewUserAndData cenv $ \username _ userToken ->
            runClientOrError cenv $ do
              NoContent <- clientAdminPutAccountSubscription adminToken username end
              accountInfo <- clientGetAccountInfo userToken
              liftIO $ accountInfoStatus accountInfo `shouldBe` HasPaid end

    it "sets the subscription time correctly the second time as well" $ \cenv ->
      forAllValid $ \end1 ->
        forAllValid $ \end2 ->
          withAdmin cenv $ \adminToken ->
            withValidNewUserAndData cenv $ \username _ userToken ->
              runClientOrError cenv $ do
                NoContent <- clientAdminPutAccountSubscription adminToken username end1
                NoContent <- clientAdminPutAccountSubscription adminToken username end2
                accountInfo <- clientGetAccountInfo userToken
                liftIO $ accountInfoStatus accountInfo `shouldBe` HasPaid end2
