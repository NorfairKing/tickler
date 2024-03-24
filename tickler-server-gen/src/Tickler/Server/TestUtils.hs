{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Tickler.Server.TestUtils
  ( withTicklerServer,
    withTicklerServerAndDatabase,
    withFreeTicklerServer,
    withFreeTicklerServerAndDatabase,
    withPaidTicklerServer,
    withPaidTicklerServerAndDatabase,
    withPaidTicklerServer_,
    withPaidTicklerServerAndDatabase_,
    withBothTicklerAndIntrayServer,
    ticklerTestClientEnvSetupFunc,
    ticklerTestClientEnvAndDatabaseSetupFunc,
    withTicklerDatabase,
    runClient,
    runClientOrError,
    testRunLooper,
    randomRegistration,
    withAdmin,
    withValidNewUser,
    withValidNewUserAndData,
    withNewUser,
    requiresAdmin,
    login,
    module Servant.Client,
  )
where

import Control.Exception
import Control.Monad.Logger
import Data.Text.Encoding (encodeUtf8)
import Data.UUID.Typed
import Database.Persist.Sqlite
import Import
import Intray.Server.TestUtils (intrayTestClientEnvSetupFunc)
import qualified Network.HTTP.Client as HTTP
import qualified Network.HTTP.Types as HTTP
import Servant
import Servant.Auth.Client
import Servant.Auth.Server as Auth
import Servant.Client
import Test.Syd.Persistent.Sqlite (connectionPoolSetupFunc)
import Test.Syd.Wai (applicationSetupFunc, managerSpec)
import Tickler.API.Gen ()
import Tickler.Client
import Tickler.Server
import Tickler.Server.Looper.Types
import Tickler.Server.OptParse.Types
import Tickler.Server.Types
import Web.Cookie

withTicklerServer :: TestDef '[HTTP.Manager] ClientEnv -> Spec
withTicklerServer specFunc = do
  describe "Free" $ withFreeTicklerServer specFunc
  describe "Paid" $ withPaidTicklerServer_ specFunc

withTicklerServerAndDatabase :: TestDef '[HTTP.Manager] (ConnectionPool, ClientEnv) -> Spec
withTicklerServerAndDatabase specFunc = do
  describe "Free" $ withFreeTicklerServerAndDatabase specFunc
  describe "Paid" $ withPaidTicklerServerAndDatabase_ specFunc

withPaidTicklerServer_ :: TestDef '[HTTP.Manager] ClientEnv -> Spec
withPaidTicklerServer_ = withPaidTicklerServer 5

withPaidTicklerServerAndDatabase_ :: TestDef '[HTTP.Manager] (ConnectionPool, ClientEnv) -> Spec
withPaidTicklerServerAndDatabase_ = withPaidTicklerServerAndDatabase 5

withPaidTicklerServer :: Int -> TestDef '[HTTP.Manager] ClientEnv -> Spec
withPaidTicklerServer maxFree =
  managerSpec
    . setupAroundWith' (\man () -> paidTicklerTestClientEnvSetupFunc maxFree man)
    . modifyMaxSuccess (`div` 20)

withPaidTicklerServerAndDatabase :: Int -> TestDef '[HTTP.Manager] (ConnectionPool, ClientEnv) -> Spec
withPaidTicklerServerAndDatabase maxFree =
  managerSpec
    . setupAroundWith' (\man () -> paidTicklerTestClientEnvAndDatabaseSetupFunc maxFree man)
    . modifyMaxSuccess (`div` 20)

withFreeTicklerServer :: TestDef '[HTTP.Manager] ClientEnv -> Spec
withFreeTicklerServer =
  managerSpec
    . setupAroundWith' (\man () -> ticklerTestClientEnvSetupFunc Nothing man)
    . modifyMaxSuccess (`div` 20)

withFreeTicklerServerAndDatabase :: TestDef '[HTTP.Manager] (ConnectionPool, ClientEnv) -> Spec
withFreeTicklerServerAndDatabase =
  managerSpec
    . setupAroundWith' (\man () -> ticklerTestClientEnvAndDatabaseSetupFunc Nothing man)
    . modifyMaxSuccess (`div` 20)

withBothTicklerAndIntrayServer :: TestDef '[HTTP.Manager] (ClientEnv, ClientEnv) -> Spec
withBothTicklerAndIntrayServer specFunc =
  managerSpec
    $ setupAroundWith' (\man () -> bothSetupFunc man)
    $ modifyMaxSuccess (`div` 20) specFunc
  where
    bothSetupFunc :: HTTP.Manager -> SetupFunc (ClientEnv, ClientEnv)
    bothSetupFunc man = do
      tcenv <- ticklerTestClientEnvSetupFunc Nothing man
      icenv <- intrayTestClientEnvSetupFunc Nothing man
      pure (tcenv, icenv)

paidTicklerTestClientEnvSetupFunc :: Int -> HTTP.Manager -> SetupFunc ClientEnv
paidTicklerTestClientEnvSetupFunc maxFree man = snd <$> paidTicklerTestClientEnvAndDatabaseSetupFunc maxFree man

paidTicklerTestClientEnvAndDatabaseSetupFunc :: Int -> HTTP.Manager -> SetupFunc (ConnectionPool, ClientEnv)
paidTicklerTestClientEnvAndDatabaseSetupFunc maxFree man = do
  let monetisationSetStripeSettings =
        StripeSettings
          { stripeSetPlan = "dummy-plan",
            stripeSetSecretKey = error "should not try to access stripe during testing",
            stripeSetPublishableKey = "Example, should not be used."
          }
  let monetisationSetPrice = "dummy price"
  let monetisationSetMaxItemsFree = maxFree
  ticklerTestClientEnvAndDatabaseSetupFunc (Just MonetisationSettings {..}) man

ticklerTestClientEnvSetupFunc :: Maybe MonetisationSettings -> HTTP.Manager -> SetupFunc ClientEnv
ticklerTestClientEnvSetupFunc mEnv man = snd <$> ticklerTestClientEnvAndDatabaseSetupFunc mEnv man

ticklerTestClientEnvAndDatabaseSetupFunc :: Maybe MonetisationSettings -> HTTP.Manager -> SetupFunc (ConnectionPool, ClientEnv)
ticklerTestClientEnvAndDatabaseSetupFunc menv man = do
  pool <- ticklerTestConnectionSetupFunc
  signingKey <- liftIO Auth.generateKey
  let jwtCfg = defaultJWTSettings signingKey
  let cookieCfg = defaultCookieSettings
  let ticklerEnv =
        TicklerServerEnv
          { envLogFunc = evaluatingLog,
            envConnectionPool = pool,
            envCookieSettings = cookieCfg,
            envJWTSettings = jwtCfg,
            envAdmins = catMaybes [parseUsername "admin"],
            envFreeloaders = catMaybes [parseUsername "freeloader"],
            envMonetisation = menv
          }
  let application = serveWithContext ticklerAPI (ticklerAppContext ticklerEnv) (makeTicklerServer ticklerEnv)
  p <- applicationSetupFunc application
  pure (pool, mkClientEnv man (BaseUrl Http "127.0.0.1" (fromIntegral p) ""))

withTicklerDatabase :: SpecWith ConnectionPool -> Spec
withTicklerDatabase = modifyMaxSuccess (`div` 5) . setupAround ticklerTestConnectionSetupFunc

ticklerTestConnectionSetupFunc :: SetupFunc ConnectionPool
ticklerTestConnectionSetupFunc = connectionPoolSetupFunc serverAutoMigration

runClient :: ClientEnv -> ClientM a -> IO (Either ClientError a)
runClient = flip runClientM

runClientOrError :: ClientEnv -> ClientM a -> IO a
runClientOrError cenv func = do
  errOrRes <- runClient cenv func
  case errOrRes of
    Left err -> expectationFailure $ show err
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
      { registrationUsername = fromJust $ parseUsername $ uuidText u1,
        registrationPassword = uuidText u2
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
      failure $ unlines ["Registration should not fail with error: ", show t]
    Left err -> failure $ unlines ["Registration should not fail with error: ", show err]
    Right NoContent -> login cenv (registrationUsername r) (registrationPassword r) >>= func

requiresAdmin :: ClientEnv -> (Token -> ClientM a) -> Expectation
requiresAdmin cenv func =
  withValidNewUser cenv $ \token -> do
    errOrStats <- runClient cenv $ func token
    case errOrStats of
      Left err ->
        case err of
          FailureResponse _ resp ->
            HTTP.statusCode (Servant.Client.responseStatusCode resp) `shouldBe` 401
          _ -> failure "Should have got a failure response."
      Right _ -> failure "Should not have been allowed."

login :: ClientEnv -> Username -> Text -> IO Token
login cenv un pw = do
  let lf = LoginForm {loginFormUsername = un, loginFormPassword = pw}
  Headers NoContent (HCons sessionHeader HNil) <- runClientOrError cenv $ clientPostLogin lf
  case sessionHeader of
    MissingHeader -> failure "Login should return a session header"
    UndecodableHeader _ -> failure "Login should return a decodable session header"
    Header session -> pure $ Token $ setCookieValue $ parseSetCookie $ encodeUtf8 session

failure :: String -> IO a
failure = expectationFailure

testRunLooper :: ConnectionPool -> Looper a -> IO a
testRunLooper pool looper = do
  -- When debugging:
  -- stdErrLog <- runStderrLoggingT askLoggerIO
  -- Evaluate the log string, but don't print it anywhere.
  runLoggingT (runLooper (LooperEnv {looperEnvPool = pool}) looper) evaluatingLog

evaluatingLog :: Loc -> LogSource -> LogLevel -> LogStr -> IO ()
evaluatingLog loc source level str = do
  _ <- evaluate loc
  _ <- evaluate source
  _ <- evaluate level
  _ <- evaluate str
  pure ()
