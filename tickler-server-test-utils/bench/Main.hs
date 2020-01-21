{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs #-}

module Main
  ( main
  ) where

import Criterion.Main
import Servant.API
import Servant.Client

import Tickler.Client
import Tickler.Server.TestUtils

main :: IO ()
main =
  withServer $ \cenv ->
    defaultMain
      [bench "register" $ register cenv, bench "register and login" $ registerAndLogin cenv]

register :: ClientEnv -> Benchmarkable
register cenv =
  whnfIO $ do
    r <- randomRegistration
    runClientOrError cenv $ clientPostRegister r

registerAndLogin :: ClientEnv -> Benchmarkable
registerAndLogin cenv =
  whnfIO $ do
    r <- randomRegistration
    runClientOrError cenv $ do
      NoContent <- clientPostRegister r
      clientPostLogin $ registrationLoginForm r

registrationLoginForm :: Registration -> LoginForm
registrationLoginForm Registration {..} =
  LoginForm {loginFormUsername = registrationUsername, loginFormPassword = registrationPassword}

withServer :: (ClientEnv -> IO ()) -> IO ()
withServer func = (,) <$> setupTestHttpManager <*> setupTicklerTestApp >>= withTicklerApp func
