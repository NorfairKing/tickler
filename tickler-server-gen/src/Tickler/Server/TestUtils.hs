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
    withFreeTicklerTestApp,
    withPaidTicklerTestApp,
    withTicklerTestApp,
    withBothTicklerAndIntrayServer,
    runClient,
    runClientOrError,
    randomRegistration,
    withAdmin,
    withValidNewUser,
    withValidNewUserAndData,
    requiresAdmin,
    login,
    module Servant.Client,
  )
where

import Control.Monad.Logger
import Control.Monad.Trans.Resource (runResourceT)
import Data.Cache as Cache
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)
import Data.Time
import Data.UUID.Typed
import Database.Persist.Sqlite
import Import
import Intray.Server.TestUtils ()
import Lens.Micro
import qualified Network.HTTP.Types as HTTP
import Network.Wai.Handler.Warp (testWithApplication)
import Servant
import Servant.Auth.Client
import Servant.Auth.Server as Auth
import Servant.Client
import Tickler.API.Gen ()
import Tickler.Client
import Tickler.Server
import Tickler.Server.Looper
import Tickler.Server.OptParse.Types
import Tickler.Server.Types
import Web.Cookie
import Web.Stripe.Plan as Stripe

withTicklerServer :: SpecWith ClientEnv -> Spec
withTicklerServer specFunc = do
  describe "Free" $ withPaidTicklerServer_ specFunc
  describe "Paid" $ withFreeTicklerServer specFunc

withPaidTicklerServer_ :: SpecWith ClientEnv -> Spec
withPaidTicklerServer_ = withPaidTicklerServer 5

withPaidTicklerServer :: Int -> SpecWith ClientEnv -> Spec
withPaidTicklerServer maxFree specFunc =
  around (withPaidTicklerTestApp maxFree) $
    modifyMaxShrinks (const 0) $
      modifyMaxSuccess (`div` 20) specFunc

withFreeTicklerServer :: SpecWith ClientEnv -> Spec
withFreeTicklerServer specFunc =
  around withFreeTicklerTestApp $
    modifyMaxShrinks (const 0) $
      modifyMaxSuccess (`div` 20) specFunc

withBothTicklerAndIntrayServer :: SpecWith (ClientEnv, ClientEnv) -> Spec
withBothTicklerAndIntrayServer specFunc =
  aroundWith withBoth $
    modifyMaxShrinks (const 0) $
      modifyMaxSuccess (`div` 20) specFunc
  where
    withBoth :: ActionWith (ClientEnv, ClientEnv) -> ActionWith ()
    withBoth func () =
      withFreeIntrayTestApp $ \icenv ->
        withFreeTicklerTestApp $ \tcenv ->
          func (tcenv, icenv)

withPaidTicklerTestApp :: Int -> (ClientEnv -> IO a) -> IO a
withPaidTicklerTestApp maxFree func = do
  now <- getCurrentTime
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
  monetisationEnvPlanCache <- newCache Nothing
  Cache.insert monetisationEnvPlanCache planName dummyPlan
  let monetisationEnvStripeSettings =
        StripeSettings
          { stripeSetPlan = planName,
            stripeSetStripeConfig = error "should not try to access stripe during testing",
            stripeSetPublishableKey = "Example, should not be used."
          }
  let monetisationEnvMaxItemsFree = maxFree
  withTicklerTestApp (Just MonetisationEnv {..}) func

withFreeTicklerTestApp :: (ClientEnv -> IO a) -> IO a
withFreeTicklerTestApp = withTicklerTestApp Nothing

withTicklerTestApp :: Maybe MonetisationEnv -> (ClientEnv -> IO a) -> IO a
withTicklerTestApp menv func = withTicklerTestConn $ \pool -> do
  man <- setupTestHttpManager
  jwtCfg <- defaultJWTSettings <$> Auth.generateKey
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
  let app = serveWithContext ticklerAPI (ticklerAppContext ticklerEnv) (makeTicklerServer ticklerEnv)
  testWithApplication (pure app) $ \port ->
    func $ ClientEnv man (BaseUrl Http "127.0.0.1" port "") Nothing

testdbFile :: String
testdbFile = "tickler-test.db"

withTicklerTestConn :: (ConnectionPool -> IO a) -> IO a
withTicklerTestConn func =
  withSystemTempDir "tickler-server" $ \tdir -> do
    dbPath <- resolveFile tdir testdbFile
    let connInfo = mkSqliteConnectionInfo (T.pack (fromAbsFile dbPath)) & walEnabled .~ False
    runNoLoggingT $ do
      p <- createSqlitePoolFromInfo connInfo 1
      void $ runResourceT $ flip runSqlPool p $ runMigrationQuiet migrateAll
      liftIO $ func p

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
failure s = do
  expectationFailure s
  undefined -- Won't get here anyway
