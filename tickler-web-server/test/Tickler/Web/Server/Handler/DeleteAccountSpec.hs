module Tickler.Web.Server.Handler.DeleteAccountSpec where

import TestImport

import Network.HTTP.Types

import Yesod.Test

import Tickler.Web.Server.Foundation
import Tickler.Web.Server.TestUtils

spec :: Spec
spec =
    ticklerWebServerSpec $
    ydescribe "DeleteAccount" $
    yit "deletes account sucessfully and is then logged out" $
    withExampleAccountAndLogin_ $ do
        get AccountR
        statusIs 200
        request $ do
            setMethod methodPost
            setUrl AccountDeleteR
            addTokenFromCookie
        statusIs 303
        loc <- getLocation
        case loc of
            Right r -> liftIO $ r `shouldBe` HomeR
            _ ->
                liftIO $
                expectationFailure $
                unwords ["Should have redirected:", show loc]
