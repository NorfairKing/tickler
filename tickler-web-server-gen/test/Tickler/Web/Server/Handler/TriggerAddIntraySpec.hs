{-# LANGUAGE OverloadedStrings #-}

module Tickler.Web.Server.Handler.TriggerAddIntraySpec where

import qualified Data.Set as S
import qualified Data.Text as T
import qualified Intray.Client as Intray
import Intray.Server.TestUtils (intrayTestClientEnvSetupFunc)
import qualified Intray.Server.TestUtils as Intray
import Test.Syd.Yesod
import TestImport
import Tickler.Client
import Tickler.Web.Server.Foundation
import Tickler.Web.Server.TestUtils

spec :: Spec
spec =
  ticklerWebServerSpec $ do
    it "fails to add an intray trigger if the intray server is down" $ \yc ->
      forAllValid $ \intrayUsername ->
        runYesodClientM yc $
          withExampleAccountAndLogin_ $ do
            intrayBaseUrl <- parseBaseUrl "intray.example.com"

            get TriggersR
            statusIs 200
            request $ do
              setMethod methodPost
              setUrl TriggerAddIntrayR
              addTokenFromCookie
              addPostParam "url" $ T.pack $ showBaseUrl intrayBaseUrl
              addPostParam "username" $ Intray.usernameText intrayUsername
              -- Doesn't matter, doesn't exist anyway
              addPostParam "access-key" "82fd3442102a075a65cc17d5fd71c2bb"
            statusIs 303
            locationShouldBe TriggersR
            _ <- followRedirect
            statusIs 200

    itWithOuter "can post an intray trigger" $ \yc man ->
      forAllValid $ \accessKeyName -> do
        unSetupFunc (intrayTestClientEnvSetupFunc Nothing man) $ \ienv ->
          Intray.withValidNewUserAndData ienv $ \intrayUsername _ itoken -> do
            -- Add an intray access key that only permits adding items
            accessKeyCreated <-
              Intray.runClientOrError ienv $
                Intray.clientPostAddAccessKey
                  itoken
                  Intray.AddAccessKey
                    { Intray.addAccessKeyName = accessKeyName,
                      Intray.addAccessKeyPermissions = S.singleton Intray.PermitAdd
                    }

            runYesodClientM yc $
              withExampleAccountAndLogin_ $ do
                get TriggersR
                statusIs 200
                request $ do
                  setMethod methodPost
                  setUrl TriggerAddIntrayR
                  addTokenFromCookie
                  addPostParam "url" $ T.pack $ showBaseUrl $ baseUrl ienv
                  addPostParam "username" $ Intray.usernameText intrayUsername
                  addPostParam "access-key" $ Intray.accessKeySecretText $ Intray.accessKeyCreatedKey accessKeyCreated
                statusIs 303
                locationShouldBe TriggersR
                _ <- followRedirect
                statusIs 200
