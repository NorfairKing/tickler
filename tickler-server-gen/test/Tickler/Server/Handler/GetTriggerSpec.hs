{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Tickler.Server.Handler.GetTriggerSpec (spec) where

import Network.HTTP.Types as Http
import TestImport
import Tickler.API.Gen ()
import Tickler.Client
import Tickler.Server.TestUtils

spec :: Spec
spec =
  withTicklerServer $ do
    it "fails to get a nonexistent trigger" $ \cenv ->
      forAllValid $ \uuid ->
        withValidNewUser cenv $ \token -> do
          errOrRes <- runClient cenv $ clientGetTrigger token uuid
          case errOrRes of
            Left err ->
              case err of
                FailureResponse _ resp -> responseStatusCode resp `shouldBe` Http.notFound404
                _ -> expectationFailure $ show err
            Right _ -> expectationFailure "Should not have succeeded"
