module Tickler.Server.Handler.RetryTriggeredSpec
  ( spec,
  )
where

import TestImport
import Tickler.API.Gen ()
import Tickler.Client
import Tickler.Server.TestUtils

spec :: Spec
spec =
  withTicklerServer $
    describe "RetryTriggered" $
      it "deletes the trigger attempts on an item" $
        \cenv ->
          forAllValid $ \t ->
            withValidNewUser cenv $ \token -> do
              errOrItem <-
                runClient cenv $ do
                  uuid <- clientPostAddItem token t
                  void $ clientRetryTriggered token [uuid]
                  clientGetItem token uuid
              case errOrItem of
                Left err -> expectationFailure $ show err
                Right ii ->
                  case itemInfoTriggered ii of
                    Nothing -> pure ()
                    Just ti -> triggeredInfoTriggerTriggerAttempts ti `shouldBe` []
