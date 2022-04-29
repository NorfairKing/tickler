module Tickler.Server.Handler.DeleteItemSpec
  ( spec,
  )
where

import Network.HTTP.Types.Status
import Servant.Client
import TestImport
import Tickler.API.Gen ()
import Tickler.Client
import Tickler.Server.TestUtils

spec :: Spec
spec = withTicklerServer $ do
  it "fails to delete a nonexistent item" $ \cenv ->
    forAllValid $ \uuid ->
      withValidNewUser cenv $ \token -> do
        errOrResult <- runClient cenv $ clientDeleteItem token uuid
        case errOrResult of
          Left err ->
            case err of
              FailureResponse _ resp -> responseStatusCode resp `shouldBe` notFound404
              _ -> expectationFailure $ unwords ["Unexpected error:", show err]
          Right _ -> expectationFailure "Should not have succeeded."

  it "fails to delete another user's item" $ \cenv ->
    forAllValid $ \tickle ->
      withValidNewUser cenv $ \token1 -> do
        withValidNewUser cenv $ \token2 -> do
          errOrResult <- runClient cenv $ do
            uuid <- clientPostItem token1 tickle
            clientDeleteItem token2 uuid
          case errOrResult of
            Left err ->
              case err of
                FailureResponse _ resp -> responseStatusCode resp `shouldBe` notFound404
                _ -> expectationFailure $ unwords ["Unexpected error:", show err]
            Right _ -> expectationFailure "Should not have succeeded."

  it "succesfully manages to delete the item that was just added" $ \cenv ->
    forAllValid $ \tickle ->
      withValidNewUser cenv $ \token -> do
        errOrItem <-
          runClient cenv $ do
            uuid <- clientPostItem token tickle
            NoContent <- clientDeleteItem token uuid
            clientGetItem token uuid
        case errOrItem of
          Left err ->
            case err of
              FailureResponse _ resp -> responseStatusCode resp `shouldBe` notFound404
              _ -> expectationFailure $ unwords ["Unexpected error:", show err]
          Right _ -> expectationFailure "Should not have succeeded."
