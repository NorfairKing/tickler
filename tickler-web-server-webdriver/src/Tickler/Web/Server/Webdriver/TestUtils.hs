{-# LANGUAGE DataKinds #-}

module Tickler.Web.Server.Webdriver.TestUtils where

import Control.Concurrent
import Control.Monad.Reader
import qualified Data.HashMap.Strict as HM
import Database.Persist.Sql as DB
import Network.HTTP.Client as HTTP
import Servant.Auth.Client
import Test.Syd
import Test.Syd.Webdriver
import Test.Syd.Webdriver.Yesod
import Test.Syd.Yesod
import Tickler.API
import Tickler.Server.TestUtils as API
import Tickler.Web.Server.Foundation
import Tickler.Web.Server.TestUtils as Web

-- This entire mess is only necessary because we did the sydtest-discover trick
-- to only run one selenium server.
-- And we need access to the database in order to get the verification email
-- out.

ticklerWebdriverWithDBSpec ::
  TestDef '[SeleniumServerHandle, HTTP.Manager] (DB.ConnectionPool, WebdriverTestEnv App) ->
  WebdriverSpec App
ticklerWebdriverWithDBSpec = setupAroundWith (const $ pure ()) . setupAroundWith' go2 . setupAroundWith' go1
  where
    go2 :: SeleniumServerHandle -> () -> SetupFunc SeleniumServerHandle
    go2 ssh () = pure ssh

    go1 :: HTTP.Manager -> SeleniumServerHandle -> SetupFunc (DB.ConnectionPool, WebdriverTestEnv App)
    go1 man ssh = do
      (pool, cenv) <-
        API.ticklerTestClientEnvAndDatabaseSetupFunc
          Nothing -- No monetisation.
          man
      app <- Web.appSetupFunc man cenv
      yesodClient <- yesodClientSetupFunc man app
      wte <-
        webdriverTestEnvSetupFunc
          ssh
          man
          (yesodClientSiteURI yesodClient)
          app
      pure (pool, wte)

ticklerWebdriverSpec :: WebdriverSpec App -> Spec
ticklerWebdriverSpec = webdriverYesodSpec $ \man -> do
  -- Find a way to have both monetisation and without, if we can
  cenv <- API.ticklerTestClientEnvSetupFunc Nothing man
  appSetupFunc man cenv

driveClient :: ClientM a -> WebdriverTestM App (Either ClientError a)
driveClient func = do
  man <- asks $ appHTTPManager . webdriverTestEnvApp
  burl <- asks $ appAPIBaseUrl . webdriverTestEnvApp
  let cenv = mkClientEnv man burl
  liftIO $ API.runClient cenv func

driveClientOrErr :: ClientM a -> WebdriverTestM App a
driveClientOrErr func = do
  errOrRes <- driveClient func
  case errOrRes of
    Left err -> liftIO $ expectationFailure $ show err
    Right result -> pure result

lookupUserToken :: Username -> WebdriverTestM App (Maybe Token)
lookupUserToken username = do
  tokenMapVar <- asks $ appLoginTokens . webdriverTestEnvApp
  tokenMap <- liftIO $ readMVar tokenMapVar
  pure $ HM.lookup username tokenMap

getUserToken :: Username -> WebdriverTestM App Token
getUserToken username = do
  mToken <- lookupUserToken username
  case mToken of
    Nothing -> liftIO $ expectationFailure "Expected to have a login token."
    Just token -> pure token
