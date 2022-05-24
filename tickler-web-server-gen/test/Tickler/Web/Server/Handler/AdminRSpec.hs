{-# LANGUAGE OverloadedStrings #-}

module Tickler.Web.Server.Handler.AdminRSpec where

import qualified Data.Text as T
import Data.Time
import Test.Syd.Yesod
import TestImport
import Tickler.Web.Server.Foundation
import Tickler.Web.Server.TestUtils

spec :: Spec
spec = ticklerWebServerSpec $ do
  describe "AdminR" $ do
    it "GETs a 404 when not logged in as admin" $
      withExampleAccount_ $ do
        get $ AdminR AdminPanelR
        statusIs 404

    it "GETs a 200 when logged in as admin" $
      withAdminAccountAndLogin_ $ do
        get $ AdminR AdminPanelR
        statusIs 200

  describe "AdminAccountR" $ do
    it "GETs a 404 when not logged in as admin" $
      withExampleAccount $ \username _ -> do
        get $ AdminR $ AdminAccountR username
        statusIs 404

    it "GETs a 200 when logged in as admin" $
      withExampleAccount $ \username _ -> do
        withAdminAccountAndLogin_ $ do
          get $ AdminR $ AdminAccountR username
          statusIs 200

  describe "AdminAccountDeleteR" $ do
    it "POSTs a 200 when logged in as admin" $
      withExampleAccount $ \username _ -> do
        withAdminAccountAndLogin_ $ do
          -- An account beforehand
          get $ AdminR $ AdminAccountR username
          statusIs 200
          -- Can delete it
          request $ do
            setMethod methodPost
            setUrl $ AdminR $ AdminAccountDeleteR username
            addToken
          statusIs 303
          _ <- followRedirect
          statusIs 200
          -- No more account afterwards
          get $ AdminR $ AdminAccountR username
          statusIs 404

  describe "AdminAccountSetSubscriptionR" $ do
    it "POSTs a 200 when logged in as admin" $ \yc ->
      forAllValid $ \day -> runYesodClientM yc $ do
        withExampleAccount $ \username _ -> do
          withAdminAccountAndLogin_ $ do
            -- An account beforehand
            get $ AdminR $ AdminAccountSetSubscriptionR username
            statusIs 200
            -- Can delete it
            request $ do
              setMethod methodPost
              setUrl $ AdminR $ AdminAccountSetSubscriptionR username
              addToken
              addPostParam "end-date" $ T.pack $ formatTime defaultTimeLocale "%F" (day :: Day)
            statusIs 303
            locationShouldBe $ AdminR $ AdminAccountSetSubscriptionR username
            _ <- followRedirect
            statusIs 200
