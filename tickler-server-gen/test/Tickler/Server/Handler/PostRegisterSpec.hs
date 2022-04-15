{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Tickler.Server.Handler.PostRegisterSpec (spec) where

import Network.HTTP.Types as HTTP
import Servant.API
import Servant.Client
import TestImport
import Tickler.API
import Tickler.API.Gen ()
import Tickler.Client
import Tickler.Server.TestUtils

spec :: Spec
spec = withTicklerServer $
  describe "Register" $ do
    it "does not crash" $ \cenv ->
      forAllValid $ \registration -> do
        nameOrError <- runClient cenv $ clientPostRegister registration
        case nameOrError of
          Right NoContent -> pure ()
          Left err ->
            let snf = expectationFailure $ "Should not fail with error: " <> show err
             in case err of
                  FailureResponse _ resp ->
                    if HTTP.statusCode (responseStatusCode resp) == 409
                      then pure ()
                      else snf
                  _ -> snf
    it "returns err409 when the username already exists" $ \cenv ->
      forAllValid $ \password ->
        forAllValid $ \registration -> do
          void $ runClient cenv $ clientPostRegister registration
          nameOrError <-
            runClient cenv . clientPostRegister $
              Registration (registrationUsername registration) password
          case nameOrError of
            Left err ->
              let snf = expectationFailure $ "Should not fail with error: " <> show err
               in case err of
                    FailureResponse _ resp ->
                      if HTTP.statusCode (responseStatusCode resp) == 409
                        then pure ()
                        else snf
                    _ -> snf
            Right _ -> expectationFailure "Should not have succeeded."
