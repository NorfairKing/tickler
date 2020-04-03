{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module Tickler.Server.Handler.PostItemSpec
  ( spec
  ) where

import TestImport

import Network.HTTP.Types as Http

import Tickler.Client

import Tickler.API.Gen ()
import Tickler.Server.TestUtils

spec :: Spec
spec =
  describe "PostItem" $
  withTicklerServer $ do
    it "edits an item without crashing" $ \cenv ->
      forAllValid $ \t ->
        forAllValid $ \tt ->
          withValidNewUser cenv $ \token -> do
            NoContent <-
              runClientOrError cenv $ do
                uuid <- clientPostAddItem token t
                clientPostItem token uuid tt
            pure ()
    it "doesn't allow you to edit an item that didn't exist yet" $ \cenv ->
      forAllValid $ \uuid ->
        forAllValid $ \tt ->
          withValidNewUser cenv $ \token -> do
            errOrNoContent <- runClient cenv $ clientPostItem token uuid tt
            case errOrNoContent of
              Right NoContent -> expectationFailure "Should have failed."
              Left err ->
                case err of
                  FailureResponse _ resp -> responseStatusCode resp `shouldBe` Http.notFound404
                  _ -> expectationFailure $ show err
