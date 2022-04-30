{-# LANGUAGE OverloadedStrings #-}

module Tickler.Web.Server.Handler.TriggerAddIntraySpec where

import Control.Monad.Reader
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Intray.Client as Intray
import qualified Intray.Data as Intray
import qualified Intray.Server.TestUtils as Intray
import Test.Syd.Yesod
import TestImport
import Tickler.Client
import Tickler.Web.Server.Foundation
import Tickler.Web.Server.TestUtils

spec :: Spec
spec =
  ticklerWebServerSpec $ do
    it "can post an intray trigger" $ \yc ->
      forAllValid $ \username ->
        forAllValid $ \accessKeyName -> do
          let intrayBaseUrl = appDefaultIntrayUrl $ yesodClientSite yc
          let ienv = mkClientEnv (yesodClientManager yc) intrayBaseUrl
          Intray.withValidNewUserAndData ienv $ \un _ itoken -> do
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
                  addPostParam "url" $ T.pack $ show intrayBaseUrl
                  addPostParam "username" $ usernameText username
                  addPostParam "access-key" $ Intray.accessKeySecretText $ Intray.accessKeyCreatedKey accessKeyCreated
                statusIs 303
                locationShouldBe TriggersR
                _ <- followRedirect
                statusIs 200
