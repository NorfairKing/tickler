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
spec =
  withTicklerServer $
    describe "DeleteItem" $
      it "succesfully manages to delete the item that was just added" $
        \cenv ->
          forAllValid $ \t ->
            withValidNewUser cenv $ \token -> do
              errOrItem <-
                runClient cenv $ do
                  uuid <- clientPostAddItem token t
                  void $ clientDeleteItem token uuid
                  clientGetItem token uuid
              case errOrItem of
                Left err ->
                  case err of
                    FailureResponse _ resp -> statusCode (responseStatusCode resp) `shouldBe` 404
                    _ -> expectationFailure $ unwords ["Unexpected error:", show err]
                Right _ -> expectationFailure "Should not have succeeded."
