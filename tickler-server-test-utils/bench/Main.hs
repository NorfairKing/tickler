{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs #-}

module Main where

import Criterion.Main
import qualified Data.ByteString as SB
import Servant.API
import Servant.Auth.Client
import Servant.Client
import System.Exit
import Web.Cookie

import Tickler.Client
import Tickler.Server.TestUtils

smallTextItem :: TypedItem
smallTextItem = TypedItem {itemType = TextItem, itemData = "Example Data"}

largeTextItem :: TypedItem
largeTextItem =
    smallTextItem
    {itemData = SB.concat $ replicate 100 $ itemData smallTextItem}

main :: IO ()
main =
    withServer $ \cenv ->
        defaultMain
            [ bench "register" $ register cenv
            , bench "register and login" $ registerAndLogin cenv
            ]

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

setupTestUser :: ClientEnv -> IO (Registration, Token)
setupTestUser cenv = do
    r <- randomRegistration
    NoContent <- runClientOrError cenv $ clientPostRegister r
    t <- login cenv $ registrationLoginForm r
    pure (r, t)

login :: ClientEnv -> LoginForm -> IO Token
login cenv form = do
    Headers NoContent (HCons _ (HCons sessionHeader HNil)) <-
        runClientOrError cenv $ clientPostLogin form
    case sessionHeader of
        Header session -> pure $ Token $ setCookieValue session
        _ -> die "something is wrong in the benchmark"

registrationLoginForm :: Registration -> LoginForm
registrationLoginForm Registration {..} =
    LoginForm
    { loginFormUsername = registrationUsername
    , loginFormPassword = registrationPassword
    }

withServer :: (ClientEnv -> IO ()) -> IO ()
withServer func =
    (,) <$> setupTestHttpManager<*> setupTicklerTestApp  >>= withTicklerApp func
