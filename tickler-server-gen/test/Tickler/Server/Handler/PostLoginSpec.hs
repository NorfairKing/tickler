{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}

module Tickler.Server.Handler.PostLoginSpec (spec) where

import Network.HTTP.Types as HTTP
import Servant.API (HList (..), Headers (..))
import TestImport hiding (HList (..))
import Tickler.Client
import Tickler.Server.TestUtils

spec :: Spec
spec = withTicklerServer
  $ describe "Login"
  $ do
    it "does not crash" $ \cenv ->
      forAllValid $ \registration ->
        withNewUser cenv registration $ \_ -> do
          let lf = LoginForm {loginFormUsername = registrationUsername registration, loginFormPassword = registrationPassword registration}
          Headers NoContent (HCons _ HNil) <- runClientOrError cenv $ clientPostLogin lf
          pure ()

    it "returns 401 when using an invalid password" $ \cenv ->
      forAllValid $ \registration ->
        forAll (genValid `suchThat` (/= registrationPassword registration)) $ \otherPassword ->
          withNewUser cenv registration $ \_ -> do
            let lf = LoginForm {loginFormUsername = registrationUsername registration, loginFormPassword = otherPassword}
            errOrResult <- runClient cenv $ clientPostLogin lf
            case errOrResult of
              Right _ -> expectationFailure "Should not have succeeded."
              Left err ->
                let snf = expectationFailure $ "Should not fail with error: " <> show err
                 in case err of
                      FailureResponse _ resp ->
                        if HTTP.statusCode (responseStatusCode resp) == 401
                          then pure ()
                          else snf
                      _ -> snf

    it "returns 401 when the account does not exist" $ \cenv ->
      forAllValid $ \registration -> do
        let lf = LoginForm {loginFormUsername = registrationUsername registration, loginFormPassword = registrationPassword registration}
        errOrResult <- runClient cenv $ clientPostLogin lf
        case errOrResult of
          Right _ -> expectationFailure "Should not have succeeded."
          Left err ->
            let snf = expectationFailure $ "Should not fail with error: " <> show err
             in case err of
                  FailureResponse _ resp ->
                    if HTTP.statusCode (responseStatusCode resp) == 401
                      then pure ()
                      else snf
                  _ -> snf
