{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Tickler.Server.Handler.PutItemSpec
  ( spec,
  )
where

import Network.HTTP.Types as Http
import TestImport
import Tickler.API.Gen ()
import Tickler.Client
import Tickler.Server.TestUtils

spec :: Spec
spec = withTicklerServer $ do
  it "gets the item that was edited" $ \cenv ->
    forAllValid $ \initialItem ->
      forAllValid $ \editedItem ->
        withValidNewUser cenv $ \token -> do
          runClientOrError cenv $ do
            uuid <- clientPostAddItem token initialItem
            NoContent <- clientPutItem token uuid editedItem
            item <- clientGetItem token uuid
            liftIO $ itemInfoContents item `shouldBe` editedItem

  it "cannot edit another users' item" $ \cenv ->
    forAllValid $ \initialItem ->
      forAllValid $ \editedItem ->
        withValidNewUser cenv $ \token1 -> do
          withValidNewUser cenv $ \token2 -> do
            errOrRes <- runClient cenv $ do
              uuid <- clientPostAddItem token1 initialItem
              clientPutItem token2 uuid editedItem
            case errOrRes of
              Left err ->
                case err of
                  FailureResponse _ resp -> responseStatusCode resp `shouldBe` Http.notFound404
                  _ -> expectationFailure $ show err
              Right NoContent -> expectationFailure "Should not have succeeded"

  it "doesn't allow you to edit an item that didn't exist yet" $ \cenv ->
    forAllValid $ \uuid ->
      forAllValid $ \tt ->
        withValidNewUser cenv $ \token -> do
          errOrNoContent <- runClient cenv $ clientPutItem token uuid tt
          case errOrNoContent of
            Right NoContent -> expectationFailure "Should have failed."
            Left err ->
              case err of
                FailureResponse _ resp -> responseStatusCode resp `shouldBe` Http.notFound404
                _ -> expectationFailure $ show err
