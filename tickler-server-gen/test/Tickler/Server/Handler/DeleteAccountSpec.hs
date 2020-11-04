{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Tickler.Server.Handler.DeleteAccountSpec
  ( spec,
  )
where

import qualified Network.HTTP.Types as Http
import TestImport
import Tickler.API.Gen ()
import Tickler.Client
import Tickler.Server.TestUtils

spec :: Spec
spec =
  withTicklerServer
    $ describe "DeleteAccount"
    $ it "deletes an account"
    $ \cenv ->
      withValidNewUser cenv $ \token -> do
        NoContent <- runClientOrError cenv $ clientDeleteAccount token
        errOrAccountInfo <- runClient cenv $ clientGetAccountInfo token
        case errOrAccountInfo of
          Left err ->
            case err of
              FailureResponse _ resp -> Http.statusCode (responseStatusCode resp) `shouldBe` 404
              _ -> expectationFailure "Should have gotten the right error."
          Right ai ->
            expectationFailure $
              unlines ["Should not have found account info, got this instead:", show ai]
