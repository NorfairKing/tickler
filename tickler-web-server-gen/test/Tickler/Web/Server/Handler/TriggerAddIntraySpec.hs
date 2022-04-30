{-# LANGUAGE OverloadedStrings #-}

module Tickler.Web.Server.Handler.TriggerAddIntraySpec where

import Control.Monad.Reader
import qualified Data.Text as T
import qualified Intray.Data as Intray
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
        forAllValid $ \accessKey ->
          runYesodClientM yc $
            withExampleAccountAndLogin_ $ do
              intrayBaseUrl <- asks $ appDefaultIntrayUrl . yesodClientSite
              get TriggersR
              statusIs 200
              request $ do
                setMethod methodPost
                setUrl TriggerAddIntrayR
                addToken
                addPostParam "url" $ T.pack $ show intrayBaseUrl
                addPostParam "username" $ usernameText username
                addPostParam "access-key" $ Intray.accessKeySecretText accessKey
              statusIs 303
              locationShouldBe TriggersR
              _ <- followRedirect
              statusIs 200
