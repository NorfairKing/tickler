{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Tickler.Server.Handler.PostAddItemSpec
  ( spec,
  )
where

import Network.HTTP.Types
import TestImport
import Tickler.API.Gen ()
import Tickler.Client
import Tickler.Server.TestUtils

spec :: Spec
spec =
  describe "PostAddItem" $ do
    withTicklerServerFree
      $ it "adds an item without crashing"
      $ \cenv ->
        forAllValid $ \t ->
          withValidNewUser cenv $ \token -> do
            uuid <- runClientOrError cenv $ clientPostAddItem token t
            shouldBeValid uuid
    withTicklerServerPaid 2
      $ it "fail to add an item if the user has not paid"
      $ \cenv ->
        forAllValid $ \t1 ->
          forAllValid $ \t2 ->
            forAllValid $ \t3 ->
              withValidNewUser cenv $ \token -> do
                u1 <- runClientOrError cenv $ clientPostAddItem token t1
                shouldBeValid u1
                u2 <- runClientOrError cenv $ clientPostAddItem token t2
                shouldBeValid u2
                errOrUuid <- runClient cenv $ clientPostAddItem token t3
                case errOrUuid of
                  Right u -> expectationFailure $ "Managed to add the third item: " <> show u
                  Left e ->
                    case e of
                      FailureResponse _ r -> responseStatusCode r `shouldBe` status402
                      _ -> expectationFailure $ "Unexpected error: " <> show e
