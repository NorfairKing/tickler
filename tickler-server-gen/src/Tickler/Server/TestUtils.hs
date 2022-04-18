{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Tickler.Server.TestUtils
  ( withTicklerServer,
    withFreeTicklerServer,
    withPaidTicklerServer,
    withPaidTicklerServer_,
    withBothTicklerAndIntrayServer,
    ticklerTestClientEnvSetupFunc,
    runClient,
    runClientOrError,
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

import Data.Cache as Cache
import Data.Text.Encoding (encodeUtf8)
import Data.Time
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
import Tickler.Server.Looper
import Tickler.Server.OptParse.Types
import Tickler.Server.Types
import Web.Cookie
import Web.Stripe.Plan as Stripe

withTicklerServer :: TestDef '[HTTP.Manager] ClientEnv -> Spec
withTicklerServer specFunc = do
  describe "Free" $ withFreeTicklerServer specFunc
  describe "Paid" $ withPaidTicklerServer_ specFunc

withPaidTicklerServer_ :: TestDef '[HTTP.Manager] ClientEnv -> Spec
withPaidTicklerServer_ = withPaidTicklerServer 5

withPaidTicklerServer :: Int -> TestDef '[HTTP.Manager] ClientEnv -> Spec
withPaidTicklerServer maxFree specFunc =
  managerSpec $
    setupAroundWith' (\man () -> paidTicklerTestClientEnvSetupFunc maxFree man) $
      modifyMaxSuccess (`div` 20) specFunc

withFreeTicklerServer :: TestDef '[HTTP.Manager] ClientEnv -> Spec
withFreeTicklerServer specFunc =
  managerSpec $
    setupAroundWith' (\man () -> ticklerTestClientEnvSetupFunc Nothing man) $
      modifyMaxSuccess (`div` 20) specFunc

withBothTicklerAndIntrayServer :: TestDef '[HTTP.Manager] (ClientEnv, ClientEnv) -> Spec
withBothTicklerAndIntrayServer specFunc =
  managerSpec $
    setupAroundWith' (\man () -> bothSetupFunc man) $
      modifyMaxSuccess (`div` 20) specFunc
  where
    bothSetupFunc :: HTTP.Manager -> SetupFunc (ClientEnv, ClientEnv)
    bothSetupFunc man = do
      tcenv <- ticklerTestClientEnvSetupFunc Nothing man
      icenv <- intrayTestClientEnvSetupFunc Nothing man
      pure (tcenv, icenv)

paidTicklerTestClientEnvSetupFunc :: Int -> HTTP.Manager -> SetupFunc ClientEnv
paidTicklerTestClientEnvSetupFunc maxFree man = do
  now <- liftIO getCurrentTime
  let planName = PlanId "dummyPlan"
      dummyPlan =
        Stripe.Plan
          { planInterval = Year,
            planName = "dummy plan",
            planCreated = now,
            planAmount = 1200,
            planCurrency = CHF,
            planId = planName,
            planObject = "plan",
            planLiveMode = False,
            planIntervalCount = Nothing,
            planTrialPeriodDays = Nothing,
            planMetaData = MetaData [],
            planDescription = Nothing
          }
  monetisationEnvPlanCache <- liftIO $ newCache Nothing
  liftIO $ Cache.insert monetisationEnvPlanCache planName dummyPlan
  let monetisationEnvStripeSettings =
        StripeSettings
          { stripeSetPlan = planName,
            stripeSetStripeConfig = error "should not try to access stripe during testing",
            stripeSetPublishableKey = "Example, should not be used."
          }
  let monetisationEnvMaxItemsFree = maxFree
  ticklerTestClientEnvSetupFunc (Just MonetisationEnv {..}) man

ticklerTestClientEnvSetupFunc :: Maybe MonetisationEnv -> HTTP.Manager -> SetupFunc ClientEnv
ticklerTestClientEnvSetupFunc menv man = ticklerTestConnectionSetupFunc >>= ticklerTestClientEnvSetupFunc' menv man

ticklerTestClientEnvSetupFunc' :: Maybe MonetisationEnv -> HTTP.Manager -> ConnectionPool -> SetupFunc ClientEnv
ticklerTestClientEnvSetupFunc' menv man pool = do
  signingKey <- liftIO Auth.generateKey
  let jwtCfg = defaultJWTSettings signingKey
  let cookieCfg = defaultCookieSettings
  let ticklerEnv =
        TicklerServerEnv
          { envConnectionPool = pool,
            envCookieSettings = cookieCfg,
            envJWTSettings = jwtCfg,
            envAdmins = catMaybes [parseUsername "admin"],
            envFreeloaders = catMaybes [parseUsername "freeloader"],
            envMonetisation = menv,
            envLoopersHandle =
              LoopersHandle
                { emailerLooperHandle = LooperHandleDisabled,
                  triggererLooperHandle = LooperHandleDisabled,
                  verificationEmailConverterLooperHandle = LooperHandleDisabled,
                  triggeredIntrayItemSchedulerLooperHandle = LooperHandleDisabled,
                  triggeredIntrayItemSenderLooperHandle = LooperHandleDisabled,
                  triggeredEmailSchedulerLooperHandle = LooperHandleDisabled,
                  triggeredEmailConverterLooperHandle = LooperHandleDisabled,
                  adminNotificationEmailConverterLooperHandle = LooperHandleDisabled,
                  stripeEventsFetcherLooperHandle = LooperHandleDisabled,
                  stripeEventsRetrierLooperHandle = LooperHandleDisabled
                }
          }
  let application = serveWithContext ticklerAPI (ticklerAppContext ticklerEnv) (makeTicklerServer ticklerEnv)
  p <- applicationSetupFunc application
  pure $ mkClientEnv man (BaseUrl Http "127.0.0.1" (fromIntegral p) "")

ticklerTestConnectionSetupFunc :: SetupFunc ConnectionPool
ticklerTestConnectionSetupFunc = connectionPoolSetupFunc migrateAll

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
