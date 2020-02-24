{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}

module Tickler.Server.TestUtils
  ( withTicklerServer
  , setupTicklerTestConn
  , setupTestHttpManager
  , setupTicklerTestApp
  , withTicklerApp
  , cleanupTicklerTestServer
  , withBothTicklerAndIntrayServer
  , runClient
  , runClientOrError
  , randomRegistration
  , withAdmin
  , withValidNewUser
  , withValidNewUserAndData
  , requiresAdmin
  , module Servant.Client
  ) where

import Import

import Control.Monad.Logger
import Control.Monad.Trans.Resource (runResourceT)
import qualified Data.Text as T
import Data.UUID.Typed
import Lens.Micro
import qualified Network.HTTP.Client as HTTP
import qualified Network.HTTP.Types as HTTP
import Web.Cookie

import Servant
import Servant.Auth.Client
import Servant.Auth.Server as Auth
import Servant.Client

import Database.Persist.Sqlite
import Network.Wai as Wai
import Network.Wai.Handler.Warp (testWithApplication)

import Intray.Server.TestUtils (cleanupIntrayTestServer, setupIntrayTestApp)

import Tickler.API
import Tickler.Client
import Tickler.Data
import Tickler.Server
import Tickler.Server.Looper
import Tickler.Server.Types

import Tickler.API.Gen ()

withTicklerServer :: SpecWith ClientEnv -> Spec
withTicklerServer specFunc =
  afterAll_ cleanupTicklerTestServer $
  beforeAll ((,) <$> setupTestHttpManager <*> setupTicklerTestApp) $
  aroundWith withTicklerApp $ modifyMaxSuccess (`div` 20) specFunc

withBothTicklerAndIntrayServer :: SpecWith (ClientEnv, ClientEnv) -> Spec
withBothTicklerAndIntrayServer specFunc =
  afterAll_ cleanupTicklerTestServer $
  afterAll_ cleanupIntrayTestServer $
  beforeAll ((,) <$> setupTicklerTestApp <*> setupIntrayTestApp Nothing) $
  aroundWith withBoth $ modifyMaxSuccess (`div` 20) specFunc
  where
    withBoth ::
         ActionWith (ClientEnv, ClientEnv) -> ActionWith (Application, (HTTP.Manager, Application))
    withBoth func (tapp, (man, iapp)) =
      testWithApplication (pure tapp) $ \tport ->
        testWithApplication (pure iapp) $ \iport ->
          func
            ( ClientEnv man (BaseUrl Http "127.0.0.1" tport "") Nothing
            , ClientEnv man (BaseUrl Http "127.0.0.1" iport "") Nothing)

testdbFile :: String
testdbFile = "tickler-test.db"

setupTicklerTestConn :: IO ConnectionPool
setupTicklerTestConn = do
  let connInfo = mkSqliteConnectionInfo (T.pack testdbFile) & walEnabled .~ False
  runNoLoggingT $ do
    p <- createSqlitePoolFromInfo connInfo 4
    void $ runResourceT $ flip runSqlPool p $ runMigrationSilent migrateAll
    pure p

setupTestHttpManager :: IO HTTP.Manager
setupTestHttpManager = HTTP.newManager HTTP.defaultManagerSettings

setupTicklerTestApp :: IO Wai.Application
setupTicklerTestApp = do
  pool <- setupTicklerTestConn
  jwtCfg <- defaultJWTSettings <$> Auth.generateKey
  let cookieCfg = defaultCookieSettings
  let ticklerEnv =
        TicklerServerEnv
          { envConnectionPool = pool
          , envCookieSettings = cookieCfg
          , envJWTSettings = jwtCfg
          , envAdmins = [fromJust $ parseUsername "admin"]
          , envLoopersHandle =
              LoopersHandle
                { emailerLooperHandle = LooperHandleDisabled
                , triggererLooperHandle = LooperHandleDisabled
                , verificationEmailConverterLooperHandle = LooperHandleDisabled
                , triggeredIntrayItemSchedulerLooperHandle = LooperHandleDisabled
                , triggeredIntrayItemSenderLooperHandle = LooperHandleDisabled
                , triggeredEmailSchedulerLooperHandle = LooperHandleDisabled
                , triggeredEmailConverterLooperHandle = LooperHandleDisabled
                }
          }
  pure $ serveWithContext ticklerAPI (ticklerAppContext ticklerEnv) (makeTicklerServer ticklerEnv)

withTicklerApp :: (ClientEnv -> IO ()) -> (HTTP.Manager, Wai.Application) -> IO ()
withTicklerApp func (man, app) =
  testWithApplication (pure app) $ \port ->
    func $ ClientEnv man (BaseUrl Http "127.0.0.1" port "") Nothing

cleanupTicklerTestServer :: IO ()
cleanupTicklerTestServer = do
  f <- resolveFile' testdbFile
  ignoringAbsence $ removeFile f

runClient :: ClientEnv -> ClientM a -> IO (Either ClientError a)
runClient = flip runClientM

runClientOrError :: ClientEnv -> ClientM a -> IO a
runClientOrError cenv func = do
  errOrRes <- runClient cenv func
  case errOrRes of
    Left err -> do
      expectationFailure $ show err
      undefined -- Won't get here anyway ^
    Right res -> pure res

withAdmin :: ClientEnv -> (Token -> IO ()) -> Expectation
withAdmin cenv = withNewUser cenv (Registration (fromJust $ parseUsername "admin") "admin")

withValidNewUser :: ClientEnv -> (Token -> IO ()) -> Expectation
withValidNewUser cenv func = withValidNewUserAndData cenv $ \_ _ -> func

randomRegistration :: IO Registration
randomRegistration = do
  u1 <- nextRandomUUID :: IO (UUID Username) -- Dummy's that are significantly likely to be random enough
  u2 <- nextRandomUUID :: IO (UUID Text)
  pure
    Registration
      { registrationUsername = fromJust $ parseUsername $ uuidText u1
      , registrationPassword = uuidText u2
      }

withValidNewUserAndData :: ClientEnv -> (Username -> Text -> Token -> IO ()) -> Expectation
withValidNewUserAndData cenv func = do
  r <- randomRegistration
  withNewUser cenv r $ func (registrationUsername r) (registrationPassword r)

withNewUser :: ClientEnv -> Registration -> (Token -> IO ()) -> Expectation
withNewUser cenv r func = do
  errOrUUID <- runClient cenv $ clientPostRegister r
  case errOrUUID of
    Left (ConnectionError t) ->
      expectationFailure $ unlines ["Registration should not fail with error: ", show t]
    Left err -> expectationFailure $ unlines ["Registration should not fail with error: ", show err]
    Right NoContent -> do
      let lf =
            LoginForm
              { loginFormUsername = registrationUsername r
              , loginFormPassword = registrationPassword r
              }
      Headers NoContent (HCons _ (HCons sessionHeader HNil)) <-
        runClientOrError cenv $ clientPostLogin lf
      case sessionHeader of
        MissingHeader -> expectationFailure "Login should return a session header"
        UndecodableHeader _ -> expectationFailure "Login should return a decodable session header"
        Header session -> func $ Token $ setCookieValue session

requiresAdmin :: ClientEnv -> (Token -> ClientM a) -> Expectation
requiresAdmin cenv func =
  withValidNewUser cenv $ \token -> do
    errOrStats <- runClient cenv $ func token
    case errOrStats of
      Left err ->
        case err of
          FailureResponse _ resp ->
            HTTP.statusCode (Servant.Client.responseStatusCode resp) `shouldBe` 401
          _ -> expectationFailure "Should have got a failure response."
      Right _ -> expectationFailure "Should not have been allowed."
