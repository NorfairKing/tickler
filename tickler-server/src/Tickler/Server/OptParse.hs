{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Tickler.Server.OptParse
  ( module Tickler.Server.OptParse,
    module Tickler.Server.OptParse.Types,
  )
where

import Control.Applicative
import Control.Monad.Logger
import qualified Control.Monad.Trans.AWS as AWS
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Database.Persist.Sqlite
import Import
import Options.Applicative
import qualified System.Environment as System
import Text.Read
import Tickler.API
import Tickler.Server.OptParse.Types
import Web.Stripe.Client as Stripe
import Web.Stripe.Types as Stripe
import YamlParse.Applicative (confDesc, readConfigFile)

getInstructions :: IO Instructions
getInstructions = do
  Arguments cmd flags <- getArguments
  env <- getEnvironment
  config <- getConfiguration flags env
  combineToInstructions cmd flags env config

combineToInstructions :: Command -> Flags -> Environment -> Maybe Configuration -> IO Instructions
combineToInstructions (CommandServe ServeFlags {..}) Flags {..} Environment {..} mConf = do
  let mc :: (Configuration -> Maybe a) -> Maybe a
      mc func = mConf >>= func
  let serveSetPort = fromMaybe 8001 $ serveFlagPort <|> envPort <|> mc confPort
  let serveSetLogLevel = fromMaybe LevelInfo $ serveFlagLogLevel <|> envLogLevel <|> mc confLogLevel
  let mWebHost = serveFlagWebHost <|> envWebHost <|> mc confWebHost
  let serveSetConnectionInfo = mkSqliteConnectionInfo $ fromMaybe "tickler.db" $ serveFlagDb <|> envDb <|> mc confDb
  let serveSetAdmins = serveFlagAdmins ++ fromMaybe [] (mc confAdmins)
  let serveSetFreeloaders = serveFlagFreeloaders ++ fromMaybe [] (mc confFreeloaders)
  serveSetLoopersSettings <-
    combineToLoopersSettings
      mWebHost
      serveFlagsLooperFlags
      envLoopersEnvironment
      (mc confLoopersConfiguration)
  serveSetMonetisationSettings <-
    do
      let (defEnabled, defStaticConfig) =
            defaultLooperSettings
              serveFlagsLooperFlags
              envLoopersEnvironment
              (mc confLoopersConfiguration)
          comb' = combineToLooperSettings' defEnabled defStaticConfig
      let MonetisationFlags {..} = serveFlagsMonetisationFlags
      let MonetisationEnvironment {..} = envMonetisationEnvironment
      let mmc :: (MonetisationConfiguration -> Maybe a) -> Maybe a
          mmc func = mc confMonetisationConfiguration >>= func
      let plan =
            Stripe.PlanId . T.pack
              <$> ( monetisationFlagStripePlan <|> monetisationEnvStripePlan
                      <|> mmc monetisationConfStripePlan
                  )
      let config =
            ( \sk ->
                StripeConfig
                  { Stripe.secretKey = StripeKey $ TE.encodeUtf8 $ T.pack sk,
                    stripeEndpoint = Nothing
                  }
            )
              <$> ( monetisationFlagStripeSecretKey <|> monetisationEnvStripeSecretKey
                      <|> mmc monetisationConfStripeSecretKey
                  )
      let publicKey =
            T.pack
              <$> ( monetisationFlagStripePublishableKey <|> monetisationEnvStripePulishableKey
                      <|> mmc monetisationConfStripePulishableKey
                  )
      monetisationSetStripeEventsFetcher <-
        comb'
          monetisationFlagLooperStripeEventsFetcher
          monetisationEnvLooperStripeEventsFetcher
          (mmc monetisationConfLooperStripeEventsFetcher)
      monetisationSetStripeEventsRetrier <-
        comb'
          monetisationFlagLooperStripeEventsRetrier
          monetisationEnvLooperStripeEventsRetrier
          (mmc monetisationConfLooperStripeEventsRetrier)
      let monetisationSetMaxItemsFree =
            fromMaybe 5 $ monetisationFlagMaxItemsFree <|> monetisationEnvMaxItemsFree
      pure $
        MonetisationSettings <$> (StripeSettings <$> plan <*> config <*> publicKey)
          <*> pure monetisationSetStripeEventsFetcher
          <*> pure monetisationSetStripeEventsRetrier
          <*> pure monetisationSetMaxItemsFree
  pure $ Instructions (DispatchServe ServeSettings {..}) Settings

defaultLooperSettings ::
  LoopersFlags -> LoopersEnvironment -> Maybe LoopersConfiguration -> (Bool, LooperStaticConfig)
defaultLooperSettings LoopersFlags {..} LoopersEnvironment {..} mConf =
  let mc :: (LoopersConfiguration -> Maybe a) -> Maybe a
      mc func = mConf >>= func
      defEnabled =
        fromMaybe True $
          looperFlagDefaultEnabled <|> looperEnvDefaultEnabled <|> mc looperConfDefaultEnabled
      defPeriod =
        fromMaybe 60 $
          looperFlagDefaultPeriod <|> looperEnvDefaultPeriod <|> mc looperConfDefaultPeriod
      defDelay =
        fromMaybe 1000000 $
          looperFlagDefaultRetryDelay <|> looperEnvDefaultRetryDelay
            <|> mc looperConfDefaultRetryDelay
      defAmount =
        fromMaybe 7 $
          looperFlagDefaultRetryAmount <|> looperEnvDefaultRetryAmount
            <|> mc looperConfDefaultRetryAmount
      defStaticConfig =
        LooperStaticConfig
          { looperStaticConfigPeriod = defPeriod,
            looperStaticConfigRetryPolicy =
              LooperRetryPolicy
                { looperRetryPolicyDelay = defDelay,
                  looperRetryPolicyAmount = defAmount
                }
          }
   in (defEnabled, defStaticConfig)

combineToLoopersSettings ::
  Maybe Text -> LoopersFlags -> LoopersEnvironment -> Maybe LoopersConfiguration -> IO LoopersSettings
combineToLoopersSettings mWebHost lf@LoopersFlags {..} le@LoopersEnvironment {..} mConf = do
  let mc :: (LoopersConfiguration -> Maybe a) -> Maybe a
      mc func = mConf >>= func
      (defEnabled, defStaticConfig) = defaultLooperSettings lf le mConf
      comb = combineToLooperSettings defEnabled defStaticConfig
      comb' = combineToLooperSettings' defEnabled defStaticConfig
  looperSetTriggererSets <-
    comb' looperFlagTriggererFlags looperEnvTriggererEnv (mc looperConfTriggererConf)
  looperSetEmailerSets <-
    comb
      looperFlagEmailerFlags
      looperEnvEmailerEnv
      (mc looperConfEmailerConf)
      (\_ _ _ -> pure $ Just $ EmailerSettings AWS.Discover)
  looperSetTriggeredIntrayItemSchedulerSets <-
    comb'
      looperFlagTriggeredIntrayItemSchedulerFlags
      looperEnvTriggeredIntrayItemSchedulerEnv
      (mc looperConfTriggeredIntrayItemSchedulerConf)
  looperSetTriggeredIntrayItemSenderSets <-
    comb'
      looperFlagTriggeredIntrayItemSenderFlags
      looperEnvTriggeredIntrayItemSenderEnv
      (mc looperConfTriggeredIntrayItemSenderConf)
  looperSetVerificationEmailConverterSets <-
    comb
      looperFlagVerificationEmailConverterFlags
      looperEnvVerificationEmailConverterEnv
      (mc looperConfVerificationEmailConverterConf)
      $ \f e c -> do
        let mea = f <|> e <|> (c >>= verificationEmailConverterConfFromAddress)
        pure $ do
          ea <- mea
          webHost <- mWebHost
          pure
            VerificationEmailConverterSettings
              { verificationEmailConverterSetFromAddress = ea,
                verificationEmailConverterSetFromName = "Tickler Verification",
                verificationEmailConverterSetWebHost = webHost
              }
  looperSetTriggeredEmailSchedulerSets <-
    comb'
      looperFlagTriggeredEmailSchedulerFlags
      looperEnvTriggeredEmailSchedulerEnv
      (mc looperConfTriggeredEmailSchedulerConf)
  looperSetTriggeredEmailConverterSets <-
    comb
      looperFlagTriggeredEmailConverterFlags
      looperEnvTriggeredEmailConverterEnv
      (mc looperConfTriggeredEmailConverterConf)
      $ \f e c -> do
        let mea = f <|> e <|> (c >>= triggeredEmailConverterConfFromAddress)
        pure $ do
          ea <- mea
          webHost <- mWebHost
          pure
            TriggeredEmailConverterSettings
              { triggeredEmailConverterSetFromAddress = ea,
                triggeredEmailConverterSetFromName = "Tickler Triggerer",
                triggeredEmailConverterSetWebHost = webHost
              }
  looperSetAdminNotificationEmailConverterSets <-
    comb
      looperFlagAdminNotificationEmailConverterFlags
      looperEnvAdminNotificationEmailConverterEnv
      (mc looperConfAdminNotificationEmailConverterConf)
      $ \AdminNotificationEmailConverterFlags {..} AdminNotificationEmailConverterEnvironment {..} mANECConf -> do
        let malc :: (AdminNotificationEmailConverterConf -> Maybe a) -> Maybe a
            malc f = mANECConf >>= f
        let mFromAddress = adminNotificationEmailConverterFlagFromAddress <|> adminNotificationEmailConverterEnvFromAddress <|> malc adminNotificationEmailConverterConfFromAddress
        let mToAddress = adminNotificationEmailConverterFlagToAddress <|> adminNotificationEmailConverterEnvFromAddress <|> malc adminNotificationEmailConverterConfFromAddress
        pure $ do
          fromAddress <- mFromAddress
          toAddress <- mToAddress
          webHost <- mWebHost
          pure
            AdminNotificationEmailConverterSettings
              { adminNotificationEmailConverterSetFromAddress = fromAddress,
                adminNotificationEmailConverterSetFromName = "Tickler Admin Notification",
                adminNotificationEmailConverterSetToAddress = toAddress,
                adminNotificationEmailConverterSetToName = "Tickler Admin",
                adminNotificationEmailConverterSetWebHost = webHost
              }
  pure LoopersSettings {..}

combineToLooperSettings' ::
  Bool ->
  LooperStaticConfig ->
  LooperFlagsWith () ->
  LooperEnvWith () ->
  Maybe (LooperConfWith ()) ->
  IO (LooperSetsWith ())
combineToLooperSettings' defEnabled defStatic flags env conf =
  combineToLooperSettings defEnabled defStatic flags env conf $ \() () _ -> pure (Just ())

combineToLooperSettings ::
  forall a b c d.
  Bool ->
  LooperStaticConfig ->
  LooperFlagsWith a ->
  LooperEnvWith b ->
  Maybe (LooperConfWith c) ->
  (a -> b -> Maybe c -> IO (Maybe d)) ->
  IO (LooperSetsWith d)
combineToLooperSettings defEnabled defStatic LooperFlagsWith {..} LooperEnvWith {..} mLooperConf func = do
  let mlc :: (LooperConfWith c -> Maybe e) -> Maybe e
      mlc f = mLooperConf >>= f
  let enabled = fromMaybe defEnabled $ looperFlagEnable <|> looperEnvEnable <|> mlc looperConfEnable
  mSets <- func looperFlags looperEnv (mlc looperConf)
  case mSets of
    Nothing -> pure LooperDisabled
    Just sets ->
      if enabled
        then do
          let LooperFlagsRetryPolicy {..} = looperFlagsRetryPolicy
          let LooperEnvRetryPolicy {..} = looperEnvRetryPolicy
              mlrpc :: (LooperConfRetryPolicy -> Maybe e) -> Maybe e
              mlrpc f = mlc looperConfRetryPolicy >>= f
          let static =
                LooperStaticConfig
                  { looperStaticConfigPeriod =
                      fromMaybe (looperStaticConfigPeriod defStatic) $
                        looperFlagsPeriod <|> looperEnvPeriod <|> mlc looperConfPeriod,
                    looperStaticConfigRetryPolicy =
                      LooperRetryPolicy
                        { looperRetryPolicyDelay =
                            fromMaybe (looperRetryPolicyDelay $ looperStaticConfigRetryPolicy defStatic) $
                              looperFlagsRetryDelay <|> looperEnvRetryDelay <|> mlrpc looperConfRetryDelay,
                          looperRetryPolicyAmount =
                            fromMaybe
                              (looperRetryPolicyAmount $ looperStaticConfigRetryPolicy defStatic)
                              $ looperFlagsRetryAmount <|> looperEnvRetryAmount
                                <|> mlrpc looperConfRetryAmount
                        }
                  }
          pure $ LooperEnabled static sets
        else pure LooperDisabled

getConfiguration :: Flags -> Environment -> IO (Maybe Configuration)
getConfiguration Flags {..} Environment {..} = do
  configFile <-
    case flagConfigFile <|> envConfigFile of
      Nothing -> getDefaultConfigFile
      Just cf -> resolveFile' cf
  readConfigFile configFile

getDefaultConfigFile :: IO (Path Abs File)
getDefaultConfigFile = do
  configDir <- getXdgDir XdgConfig (Just [reldir|tickler|])
  resolveFile configDir "config.yaml"

getEnvironment :: IO Environment
getEnvironment = do
  env <- System.getEnvironment
  let envConfigFile = getEnv env "CONFIG_FILE"
  let envDb = T.pack <$> getEnv env "DATABASE"
  let envWebHost = T.pack <$> getEnv env "WEB_HOST"
  envPort <- readEnv env "PORT"
  envLogLevel <- readEnv env "LOG_LEVEL"
  envMonetisationEnvironment <- getMonetisationEnv env
  envLoopersEnvironment <- getLoopersEnv env
  pure Environment {..}

getMonetisationEnv :: [(String, String)] -> IO MonetisationEnvironment
getMonetisationEnv env = do
  let monetisationEnvStripePlan = getEnv env "STRIPE_PLAN"
  let monetisationEnvStripeSecretKey = getEnv env "STRIPE_SECRET_KEY"
  let monetisationEnvStripePulishableKey = getEnv env "STRIPE_PUBLISHABLE_KEY"
  monetisationEnvLooperStripeEventsFetcher <- getLooperEnvWith env "STRIPE_EVENTS_FETCHER" $ pure ()
  monetisationEnvLooperStripeEventsRetrier <- getLooperEnvWith env "STRIPE_EVENTS_RETRIER" $ pure ()
  monetisationEnvMaxItemsFree <- readEnv env "MAX_ITEMS_FREE"
  pure MonetisationEnvironment {..}

getLoopersEnv :: [(String, String)] -> IO LoopersEnvironment
getLoopersEnv env = do
  looperEnvDefaultEnabled <- readEnv env "LOOPERS_DEFAULT_ENABLED"
  looperEnvDefaultPeriod <- readEnv env "LOOPERS_DEFAULT_PERIOD"
  looperEnvDefaultRetryDelay <- readEnv env "LOOPERS_DEFAULT_RETRY_DELAY"
  looperEnvDefaultRetryAmount <- readEnv env "LOOPERS_DEFAULT_RETRY_AMOUNT"
  looperEnvTriggererEnv <- getLooperEnvWith env "TRIGGERER" $ pure ()
  looperEnvEmailerEnv <- getLooperEnvWith env "EMAILER" $ pure ()
  looperEnvTriggeredIntrayItemSchedulerEnv <-
    getLooperEnvWith env "TRIGGERED_INTRAY_ITEM_SCHEDULER" $ pure ()
  looperEnvTriggeredIntrayItemSenderEnv <-
    getLooperEnvWith env "TRIGGERED_INTRAY_ITEM_SENDER" $ pure ()
  looperEnvVerificationEmailConverterEnv <-
    getLooperEnvWith env "VERIFICATION_EMAIL_CONVERTER" $
      getEitherEnv env emailValidateFromString "VERIFICATION_EMAIL_ADDRESS"
  looperEnvTriggeredEmailSchedulerEnv <- getLooperEnvWith env "TRIGGERED_EMAIL_SCHEDULER" $ pure ()
  looperEnvTriggeredEmailConverterEnv <-
    getLooperEnvWith env "TRIGGERED_EMAIL_CONVERTER" $
      getEitherEnv env emailValidateFromString "TRIGGERED_EMAIL_ADDRESS"
  looperEnvAdminNotificationEmailConverterEnv <-
    getLooperEnvWith env "ADMIN_NOTIFICATION_EMAIL_CONVERTER" $
      AdminNotificationEmailConverterEnvironment
        <$> getEitherEnv env emailValidateFromString "ADMIN_NOTIFICATION_FROM_EMAIL_ADDRESS"
        <*> getEitherEnv env emailValidateFromString "ADMIN_NOTIFICATION_TO_EMAIL_ADDRESS"
  pure LoopersEnvironment {..}

getLooperEnvWith :: [(String, String)] -> String -> IO a -> IO (LooperEnvWith a)
getLooperEnvWith env name func = do
  looperEnvEnable <- readEnv env $ intercalate "_" ["LOOPER", name, "ENABLED"]
  looperEnvPeriod <- readEnv env $ intercalate "_" ["LOOPER", name, "PERIOD"]
  looperEnvRetryPolicy <- getLooperRetryPolicyEnv env name
  looperEnv <- func
  pure LooperEnvWith {..}

getLooperRetryPolicyEnv :: [(String, String)] -> String -> IO LooperEnvRetryPolicy
getLooperRetryPolicyEnv env name = do
  looperEnvRetryDelay <- readEnv env $ intercalate "_" ["LOOPER", name, "RETRY", "DELAY"]
  looperEnvRetryAmount <- readEnv env $ intercalate "_" ["LOOPER", name, "RETRY", "AMOUNT"]
  pure LooperEnvRetryPolicy {..}

getEnv :: [(String, String)] -> String -> Maybe String
getEnv env key = lookup ("TICKLER_SERVER_" <> key) env

readEnv :: Read a => [(String, String)] -> String -> IO (Maybe a)
readEnv env key =
  forM (getEnv env key) $ \s ->
    case readMaybe s of
      Nothing -> die $ unwords ["Un-Read-able value for environment value", key <> ":", s]
      Just val -> pure val

getEitherEnv :: [(String, String)] -> (String -> Either String a) -> String -> IO (Maybe a)
getEitherEnv env func key =
  forM (getEnv env key) $ \s ->
    case func s of
      Left err ->
        die $ unlines [unwords ["Failed to parse environment variable", key <> ":", s], err]
      Right res -> pure res

getArguments :: IO Arguments
getArguments = do
  args <- System.getArgs
  let result = runArgumentsParser args
  handleParseResult result

runArgumentsParser :: [String] -> ParserResult Arguments
runArgumentsParser = execParserPure prefs_ argParser
  where
    prefs_ = defaultPrefs {prefShowHelpOnError = True, prefShowHelpOnEmpty = True}

argParser :: ParserInfo Arguments
argParser = info (helper <*> parseArgs) help_
  where
    help_ = fullDesc <> progDesc description <> confDesc @Configuration
    description = "Tickler server"

parseArgs :: Parser Arguments
parseArgs = Arguments <$> parseCommand <*> parseFlags

parseCommand :: Parser Command
parseCommand = hsubparser $ mconcat [command "serve" parseCommandServe]

parseCommandServe :: ParserInfo Command
parseCommandServe = info parser modifier
  where
    parser = CommandServe <$> parseServeFlags
    modifier = fullDesc <> progDesc "Command example." <> confDesc @Configuration

parseServeFlags :: Parser ServeFlags
parseServeFlags =
  ServeFlags
    <$> option
      (Just <$> auto)
      ( mconcat
          [long "web-host", value Nothing, metavar "HOST", help "the host to serve the web server on"]
      )
    <*> option
      (Just <$> auto)
      (mconcat [long "api-port", value Nothing, metavar "PORT", help "the port to serve the API on"])
    <*> option
      (Just <$> auto)
      (mconcat [long "log-level", value Nothing, metavar "LOG_LEVEL", help "the minimal sevirity of log levels"])
    <*> option
      (Just <$> str)
      ( mconcat
          [ long "database",
            value Nothing,
            metavar "DATABASE_CONNECTION_STRING",
            help "The sqlite connection string"
          ]
      )
    <*> many
      ( option
          (eitherReader (parseUsernameWithError . T.pack))
          (mconcat [long "admin", metavar "USERNAME", help "An admin"])
      )
    <*> many
      ( option
          (eitherReader (parseUsernameWithError . T.pack))
          ( mconcat
              [long "freeloader", metavar "USERNAME", help "A user that can use the service for free"]
          )
      )
    <*> parseMonetisationFlags
    <*> parseLoopersFlags

parseMonetisationFlags :: Parser MonetisationFlags
parseMonetisationFlags =
  MonetisationFlags
    <$> option
      (Just <$> str)
      ( mconcat
          [ long "stripe-plan",
            value Nothing,
            metavar "PLAN_ID",
            help "The product pricing plan for stripe"
          ]
      )
    <*> option
      (Just <$> str)
      ( mconcat
          [ long "stripe-secret-key",
            value Nothing,
            metavar "SECRET_KEY",
            help "The secret key for stripe"
          ]
      )
    <*> option
      (Just <$> str)
      ( mconcat
          [ long "stripe-publishable-key",
            value Nothing,
            metavar "PUBLISHABLE_KEY",
            help "The publishable key for stripe"
          ]
      )
    <*> parseLooperFlagsWith "stripe-events-fetcher" (pure ())
    <*> parseLooperFlagsWith "stripe-events-retrier" (pure ())
    <*> option
      (Just <$> auto)
      ( mconcat
          [ long "max-items-free",
            value Nothing,
            metavar "INT",
            help "How many items a user can sync in the free plan"
          ]
      )

parseLoopersFlags :: Parser LoopersFlags
parseLoopersFlags =
  LoopersFlags
    <$> onOffFlag "loopers" (help $ unwords ["enable or disable all the loopers by default"])
    <*> option
      (Just <$> auto)
      ( mconcat
          [ long "default-period",
            value Nothing,
            metavar "SECONDS",
            help "The default period for all loopers"
          ]
      )
    <*> option
      (Just <$> auto)
      ( mconcat
          [ long "default-retry-delay",
            value Nothing,
            metavar "MICROSECONDS",
            help "The retry delay for all loopers, in microseconds"
          ]
      )
    <*> option
      (Just <$> auto)
      ( mconcat
          [ long "default-retry-amount",
            value Nothing,
            metavar "AMOUNT",
            help "The default amount of times to retry a looper before failing"
          ]
      )
    <*> parseLooperFlagsWith "triggerer" (pure ())
    <*> parseLooperFlagsWith "emailer" (pure ())
    <*> parseLooperFlagsWith "intray-item-scheduler" (pure ())
    <*> parseLooperFlagsWith "intray-item-sender" (pure ())
    <*> parseLooperFlagsWith
      "verification-email-converter"
      ( option
          (Just <$> eitherReader emailValidateFromString)
          ( mconcat
              [ long "verification-email-address",
                value Nothing,
                metavar "EMAIL_ADDRESS",
                help "The email address to use to send verification emails from"
              ]
          )
      )
    <*> parseLooperFlagsWith "triggered-email-scheduler" (pure ())
    <*> parseLooperFlagsWith
      "triggered-email-converter"
      ( option
          (Just <$> eitherReader emailValidateFromString)
          ( mconcat
              [ long "triggered-email-address",
                value Nothing,
                metavar "EMAIL_ADDRESS",
                help "The email address to use to send triggered item emails from"
              ]
          )
      )
    <*> parseLooperFlagsWith
      "admin-notification-email-converter"
      parseAdminNotificationEmailConverterFlags

parseAdminNotificationEmailConverterFlags :: Parser AdminNotificationEmailConverterFlags
parseAdminNotificationEmailConverterFlags =
  AdminNotificationEmailConverterFlags
    <$> option
      (Just <$> eitherReader emailValidateFromString)
      ( mconcat
          [ long "admin-notification-from-email-address",
            value Nothing,
            metavar "EMAIL_ADDRESS",
            help "The email address to use to send admin notification emails from"
          ]
      )
    <*> option
      (Just <$> eitherReader emailValidateFromString)
      ( mconcat
          [ long "admin-notification-to-email-address",
            value Nothing,
            metavar "EMAIL_ADDRESS",
            help "The email address of the admin to use to send admin notification emails to"
          ]
      )

parseLooperFlagsWith :: String -> Parser a -> Parser (LooperFlagsWith a)
parseLooperFlagsWith name func =
  LooperFlagsWith
    <$> onOffFlag
      (intercalate "-" [name, "looper"])
      (mconcat [help $ unwords ["enable or disable the", name, "looper"]])
    <*> option
      (Just <$> auto)
      ( mconcat
          [ long $ intercalate "-" [name, "period"],
            value Nothing,
            metavar "SECONDS",
            help $ unwords ["The period for", name]
          ]
      )
    <*> parseLooperRetryPolicyFlags name
    <*> func

parseLooperRetryPolicyFlags :: String -> Parser LooperFlagsRetryPolicy
parseLooperRetryPolicyFlags name =
  LooperFlagsRetryPolicy
    <$> option
      (Just <$> auto)
      ( mconcat
          [ long $ intercalate "-" [name, "retry-delay"],
            value Nothing,
            metavar "MICROSECONDS",
            help $ unwords ["The retry delay for", name]
          ]
      )
    <*> option
      (Just <$> auto)
      ( mconcat
          [ long $ intercalate "-" [name, "retry-amount"],
            value Nothing,
            metavar "AMOUNT",
            help $ unwords ["The amount of times to retry for", name]
          ]
      )

onOffFlag :: String -> Mod FlagFields (Maybe Bool) -> Parser (Maybe Bool)
onOffFlag suffix mods =
  flag' (Just True) (mconcat [long $ pf "enable", hidden])
    <|> flag' (Just False) (mconcat [long $ pf "disable", hidden])
    <|> flag' Nothing (mconcat [long ("(enable|disable)-" ++ suffix), mods])
    <|> pure Nothing
  where
    pf s = intercalate "-" [s, suffix]

parseFlags :: Parser Flags
parseFlags =
  Flags
    <$> option
      (Just <$> str)
      (mconcat [long "config-file", value Nothing, metavar "FILEPATH", help "The config file"])
