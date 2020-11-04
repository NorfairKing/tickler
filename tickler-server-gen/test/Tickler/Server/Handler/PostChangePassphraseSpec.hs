{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Tickler.Server.Handler.PostChangePassphraseSpec
  ( spec,
  )
where

import TestImport
import Tickler.API.Gen ()
import Tickler.Client
import Tickler.Server.TestUtils

spec :: Spec
spec =
  withTicklerServer
    $ describe "PostChangePassphrase"
    $ do
      it
        "changes a users' password so that you can't log in with the old password anymore in this example"
        $ \cenv ->
          withValidNewUserAndData cenv $ \un oldPw token ->
            let newPw = "test"
             in do
                  NoContent <-
                    runClientOrError cenv
                      $ clientPostChangePassphrase token
                      $ ChangePassphrase {changePassphraseOld = oldPw, changePassphraseNew = newPw}
                  errOrRes <-
                    runClient cenv $
                      clientPostLogin LoginForm {loginFormUsername = un, loginFormPassword = oldPw}
                  case errOrRes of
                    Right _ -> expectationFailure "should not have been able to login."
                    Left _ -> pure ()
      it "changes a users' password so that you can log in with the new password in this example" $ \cenv ->
        withValidNewUserAndData cenv $ \un oldPw token ->
          let newPw = "test"
           in do
                NoContent <-
                  runClientOrError cenv
                    $ clientPostChangePassphrase token
                    $ ChangePassphrase {changePassphraseOld = oldPw, changePassphraseNew = newPw}
                token' <- login cenv un newPw
                accountInfo <- runClientOrError cenv $ clientGetAccountInfo token'
                shouldBeValid accountInfo
