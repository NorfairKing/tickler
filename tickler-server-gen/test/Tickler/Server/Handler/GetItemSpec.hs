{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Tickler.Server.Handler.GetItemSpec
  ( spec,
  )
where

import Network.HTTP.Types as Http
import TestImport
import Tickler.API
import Tickler.API.Gen ()
import Tickler.Client
import Tickler.Server.TestUtils

spec :: Spec
spec =
  withTicklerServer $ do
    it "gets the same item that was just added" $ \cenv ->
      forAllValid $ \ti ->
        withValidNewUser cenv $ \token -> do
          i <- runClientOrError cenv $ do
            uuid <- clientPostItem token ti
            clientGetItem token uuid
          itemInfoContents i `shouldBe` ti
    it "fails to get another user's item" $ \cenv ->
      forAllValid $ \ti ->
        withValidNewUser cenv $ \token1 -> do
          withValidNewUser cenv $ \token2 -> do
            uuid <- runClientOrError cenv $ clientPostItem token1 ti
            errOrRes <- runClient cenv $ clientGetItem token2 uuid
            case errOrRes of
              Left err ->
                case err of
                  FailureResponse _ resp -> responseStatusCode resp `shouldBe` Http.notFound404
                  _ -> expectationFailure $ show err
              Right _ -> expectationFailure "Should not have succeeded"
