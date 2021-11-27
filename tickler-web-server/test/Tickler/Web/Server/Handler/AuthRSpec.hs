{-# LANGUAGE OverloadedStrings #-}

module Tickler.Web.Server.Handler.AuthRSpec where

import Network.HTTP.Types
import Test.Syd.Yesod
import TestImport
import Tickler.Data (parseUsername)
import Tickler.Web.Server.Foundation
import Tickler.Web.Server.TestUtils

{-# ANN module ("HLint: ignore Reduce duplication" :: String) #-}

spec :: Spec
spec =
  ticklerWebServerSpec $
    ydescribe "RegisterR" $
      do
        yit "gets a 200" $ do
          get $ AuthR registerR
          statusIs 200
        yit "registers an example account correctly" $ do
          registerFlow "example" "example"
          statusIs 303
          loc <- getLocation
          void followRedirect
          liftIO $ loc `shouldBe` Right AddR
          statusIs 200
        yit "fails to register and shows an error if an account with the same username exists" $ do
          registerFlow "example" "example1"
          registerFlow "example" "example2"
          statusIs 303
          loc <- getLocation
          liftIO $ loc `shouldBe` Right (AuthR registerR)
          void followRedirect
          statusIs 200
          bodyContains "exists"
        yit "fails to register and shows an error if the username is not valid" $ do
          let un = "example with a space"
          liftIO $ parseUsername un `shouldBe` Nothing
          registerFlow un "example"
          statusIs 303
          loc <- getLocation
          liftIO $ loc `shouldBe` Right (AuthR registerR)
          void followRedirect
          statusIs 200
          bodyContains "Invalid"

registerFlow :: Text -> Text -> YesodExample App ()
registerFlow exampleUsername examplePassphrase = do
  get $ AuthR registerR
  statusIs 200
  request $ do
    setMethod methodPost
    setUrl $ AuthR registerR
    addTokenFromCookie
    addPostParam "username" exampleUsername
    addPostParam "passphrase" examplePassphrase
    addPostParam "passphrase-confirm" examplePassphrase
