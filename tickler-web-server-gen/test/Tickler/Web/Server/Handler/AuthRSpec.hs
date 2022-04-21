{-# LANGUAGE OverloadedStrings #-}

module Tickler.Web.Server.Handler.AuthRSpec where

import Network.HTTP.Types
import Test.Syd.Yesod
import TestImport
import Tickler.Data (parseUsername)
import Tickler.Web.Server.Foundation
import Tickler.Web.Server.TestUtils
import Yesod.Auth

{-# ANN module ("HLint: ignore Reduce duplication" :: String) #-}

spec :: Spec
spec = ticklerWebServerSpec $ do
  describe "RegisterR" $ do
    it "GETs a 200" $ do
      get $ AuthR registerR
      statusIs 200

    it "registers an example account correctly" $ do
      registerFlow "example" "example"
      statusIs 303
      loc <- getLocation
      void followRedirect
      liftIO $ loc `shouldBe` Right AddR
      statusIs 200

    it "fails to register and shows an error if an account with the same username exists" $ do
      registerFlow "example" "example1"
      registerFlow "example" "example2"
      statusIs 303
      loc <- getLocation
      liftIO $ loc `shouldBe` Right (AuthR registerR)
      void followRedirect
      statusIs 200
      bodyContains "exists"

    it "fails to register and shows an error if the username is not valid" $ do
      let un = "example with a space"
      liftIO $ parseUsername un `shouldBe` Nothing
      registerFlow un "example"
      statusIs 303
      loc <- getLocation
      liftIO $ loc `shouldBe` Right (AuthR registerR)
      void followRedirect
      statusIs 200
      bodyContains "Invalid"

  describe "LoginRR" $ do
    it "GETs a 200" $ do
      get $ AuthR LoginR
      statusIs 200

    it "fails when the account does not exist" $ do
      get $ AuthR LoginR
      statusIs 200
      request $ do
        setMethod methodPost
        setUrl $ AuthR loginFormPostTargetR
        addTokenFromCookie
        addPostParam "username" "example"
        addPostParam "passphrase" "example"
      statusIs 303
      locationShouldBe $ AuthR LoginR

    it "fails when the password is wrong" $ do
      registerFlow "example" "password"
      get $ AuthR LoginR
      statusIs 200
      request $ do
        setMethod methodPost
        setUrl $ AuthR loginFormPostTargetR
        addTokenFromCookie
        addPostParam "username" "example"
        addPostParam "passphrase" "wrong-password"
      statusIs 303
      locationShouldBe $ AuthR LoginR

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
