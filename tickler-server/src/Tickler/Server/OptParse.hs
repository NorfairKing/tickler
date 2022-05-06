{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Tickler.Server.OptParse
  ( Settings (..),
    getSettings,
  )
where

import Autodocodec.Yaml
import Control.Applicative
import Control.Monad.Logger
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Env
import Import
import Looper
import Options.Applicative as OptParse
import qualified Options.Applicative.Help as OptParse
import qualified System.Environment as System
import Text.Read
import Tickler.API
import Tickler.Server.OptParse.Types
import Web.Stripe.Client as Stripe
import Web.Stripe.Types as Stripe

getSettings :: IO Settings
getSettings = do
  flags <- getFlags
  env <- getEnvironment
  config <- getConfiguration flags env
  combineToSettings flags env config

combineToSettings :: Flags -> Environment -> Maybe Configuration -> IO Settings
combineToSettings Flags {..} Environment {..} mConf = do
  let mc :: (Configuration -> Maybe a) -> Maybe a
      mc func = mConf >>= func
  let setPort = fromMaybe 8000 $ flagPort <|> envPort <|> mc confPort
  let setLogLevel = fromMaybe LevelInfo $ flagLogLevel <|> envLogLevel <|> mc confLogLevel
  let setWebHost = fromMaybe (T.pack $ "localhost:" <> show setPort) $ flagWebHost <|> envWebHost <|> mc confWebHost
  setDb <- resolveFile' $ fromMaybe "tickler.db" $ flagDb <|> envDb <|> mc confDb
  let setAdmins = flagAdmins ++ fromMaybe [] (mc confAdmins)
  let setFreeloaders = flagFreeloaders ++ fromMaybe [] (mc confFreeloaders)
  setMonetisationSettings <- do
    let MonetisationFlags {..} = flagsMonetisationFlags
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
    let monetisationSetStripeEventsFetcher =
          deriveLooperSettings
            (seconds 8)
            (minutes 60)
            monetisationFlagLooperStripeEventsFetcher
            monetisationEnvLooperStripeEventsFetcher
            (mmc monetisationConfLooperStripeEventsFetcher)
    let monetisationSetMaxItemsFree =
          fromMaybe 5 $ monetisationFlagMaxItemsFree <|> monetisationEnvMaxItemsFree
    pure $
      MonetisationSettings
        <$> (StripeSettings <$> plan <*> config <*> publicKey)
        <*> pure monetisationSetStripeEventsFetcher
        <*> pure monetisationSetMaxItemsFree

  setTriggererFromEmailAddress <-
    maybe (die "No from email address for the email triggering configured") pure $
      flagTriggererFromEmailAddress <|> envTriggererFromEmailAddress <|> mc confTriggererFromEmailAddress
  setVerificationFromEmailAddress <-
    maybe (die "No from email address for the email trigger verification configured") pure $
      flagVerificationFromEmailAddress <|> envVerificationFromEmailAddress <|> mc confVerificationFromEmailAddress
  setAdminNotificationFromEmailAddress <-
    maybe (die "No from email address for the admin notifications configured") pure $
      flagAdminNotificationFromEmailAddress <|> envAdminNotificationFromEmailAddress <|> mc confAdminNotificationFromEmailAddress
  setAdminNotificationToEmailAddress <-
    maybe (die "No to email address for the admin notifications configured") pure $
      flagAdminNotificationToEmailAddress <|> envAdminNotificationToEmailAddress <|> mc confAdminNotificationToEmailAddress

  let setTriggererSets =
        deriveLooperSettings
          (seconds 1)
          (minutes 60)
          flagTriggererFlags
          envTriggererEnv
          (mc confTriggererConf)
  let setEmailerSets =
        deriveLooperSettings
          (seconds 2)
          (minutes 60)
          flagEmailerFlags
          envEmailerEnv
          (mc confEmailerConf)
  let setTriggeredIntrayItemSchedulerSets =
        deriveLooperSettings
          (seconds 3)
          (minutes 60)
          flagTriggeredIntrayItemSchedulerFlags
          envTriggeredIntrayItemSchedulerEnv
          (mc confTriggeredIntrayItemSchedulerConf)
  let setTriggeredIntrayItemSenderSets =
        deriveLooperSettings
          (seconds 4)
          (minutes 60)
          flagTriggeredIntrayItemSenderFlags
          envTriggeredIntrayItemSenderEnv
          (mc confTriggeredIntrayItemSenderConf)
  let setVerificationEmailConverterSets =
        deriveLooperSettings
          (seconds 5)
          (minutes 60)
          flagVerificationEmailConverterFlags
          envVerificationEmailConverterEnv
          (mc confVerificationEmailConverterConf)
  let setTriggeredEmailSchedulerSets =
        deriveLooperSettings
          (seconds 6)
          (minutes 60)
          flagTriggeredEmailSchedulerFlags
          envTriggeredEmailSchedulerEnv
          (mc confTriggeredEmailSchedulerConf)
  let setTriggeredEmailConverterSets =
        deriveLooperSettings
          (seconds 7)
          (minutes 60)
          flagTriggeredEmailConverterFlags
          envTriggeredEmailConverterEnv
          (mc confTriggeredEmailConverterConf)
  let setAdminNotificationEmailConverterSets =
        deriveLooperSettings
          (seconds 9)
          (minutes 60)
          flagAdminNotificationEmailConverterFlags
          envAdminNotificationEmailConverterEnv
          (mc confAdminNotificationEmailConverterConf)

  pure Settings {..}

getConfiguration :: Flags -> Environment -> IO (Maybe Configuration)
getConfiguration Flags {..} Environment {..} = do
  configFile <-
    case flagConfigFile <|> envConfigFile of
      Nothing -> getDefaultConfigFile
      Just cf -> resolveFile' cf
  readYamlConfigFile configFile

getDefaultConfigFile :: IO (Path Abs File)
getDefaultConfigFile = resolveFile' "config.yaml"

getEnvironment :: IO Environment
getEnvironment = Env.parse id environmentParser

environmentParser :: Env.Parser Env.Error Environment
environmentParser =
  Env.prefixed "TICKLER_SERVER_" $
    Environment
      <$> optional (Env.var Env.str "CONFIG_FILE" (Env.help "configuration file"))
      <*> optional (Env.var Env.str "DATABASE_FILE" (Env.help "database file"))
      <*> optional (Env.var Env.str "WEB_HOST" (Env.help "host that the web server is running on"))
      <*> optional (Env.var Env.auto "PORT" (Env.help "port to run the api server on"))
      <*> optional (Env.var Env.auto "LOG_LEVEL" (Env.help "minimal severity of error messages"))
      <*> monetisationEnvironmentParser
      <*> optional (Env.var Env.str "TRIGGERER_FROM_EMAIL_ADDRESS" (Env.help "From email address for triggered emails"))
      <*> optional (Env.var Env.str "VERIFICATION_FROM_EMAIL_ADDRESS" (Env.help "From email address for verification emails"))
      <*> optional (Env.var Env.str "ADMIN_NOTIFICATION_FROM_EMAIL_ADDRESS" (Env.help "From email address for admin notifcitaion emails"))
      <*> optional (Env.var Env.str "ADMIN_NOTIFICATION_TO_EMAIL_ADDRESS" (Env.help "To email address for admin notifcitaion emails"))
      <*> looperEnvironmentParser "TRIGGERER"
      <*> looperEnvironmentParser "EMAILER"
      <*> looperEnvironmentParser "TRIGGERED_INTRAY_ITEM_SCHEDULER"
      <*> looperEnvironmentParser "TRIGGERED_INTRAY_ITEM_SENDER"
      <*> looperEnvironmentParser "VERIFICATION_EMAIL_CONVERTER"
      <*> looperEnvironmentParser "TRIGGERED_EMAIL_SCHEDULER"
      <*> looperEnvironmentParser "TRIGGERED_EMAIL_CONVERTER"
      <*> looperEnvironmentParser "ADMIN_NOTIFICATION_EMAIL_CONVERTER"

monetisationEnvironmentParser :: Env.Parser Env.Error MonetisationEnvironment
monetisationEnvironmentParser =
  MonetisationEnvironment
    <$> optional (Env.var Env.str "STRIPE_PLAN" (Env.help "Stripe plan id"))
    <*> optional (Env.var Env.str "STRIPE_SECRET_KEY" (Env.help "Stripe secret key"))
    <*> optional (Env.var Env.str "STRIPE_PUBLISHABLE_KEY" (Env.help "Stripe publishable key"))
    <*> looperEnvironmentParser "STRIPE_EVENTS_FETCHER"
    <*> optional (Env.var Env.auto "MAX_ITEMS_FREE" (Env.help "Maximum number of free items"))

getEnv :: [(String, String)] -> String -> Maybe String
getEnv env key = lookup ("TICKLER_SERVER_" <> key) env

requireEnv :: IsString a => [(String, String)] -> String -> IO a
requireEnv env key = case getEnv env key of
  Nothing -> die $ "Missing env var: " <> key
  Just v -> pure $ fromString v

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

getFlags :: IO Flags
getFlags = do
  args <- System.getArgs
  let result = runFlagsParser args
  handleParseResult result

runFlagsParser :: [String] -> ParserResult Flags
runFlagsParser = execParserPure prefs_ flagsParser
  where
    prefs_ = defaultPrefs {prefShowHelpOnError = True, prefShowHelpOnEmpty = True}

flagsParser :: ParserInfo Flags
flagsParser = info (helper <*> parseFlags) help_
  where
    help_ = fullDesc <> footerDoc (Just $ OptParse.string footerStr)
    footerStr =
      unlines
        [ "Configuration file format:",
          T.unpack (TE.decodeUtf8 (renderColouredSchemaViaCodec @Configuration))
        ]

parseFlags :: Parser Flags
parseFlags =
  Flags
    <$> optional
      ( strOption
          ( mconcat
              [ long "config-file",
                metavar "FILEPATH",
                help "The config file"
              ]
          )
      )
    <*> optional
      ( option
          auto
          ( mconcat
              [ long "web-host",
                metavar "HOST",
                help "the host to serve the web server on"
              ]
          )
      )
    <*> optional
      ( option
          auto
          ( mconcat
              [ long "api-port",
                metavar "PORT",
                help "the port to serve the API on"
              ]
          )
      )
    <*> optional
      ( option
          auto
          ( mconcat
              [ long "log-level",
                metavar "LOG_LEVEL",
                help "the minimal sevirity of log messages"
              ]
          )
      )
    <*> optional
      ( strOption
          ( mconcat
              [ long "database",
                metavar "DATABASE_CONNECTION_STRING",
                help "The sqlite connection string"
              ]
          )
      )
    <*> many
      ( option
          (eitherReader (parseUsernameWithError . T.pack))
          ( mconcat
              [ long "admin",
                metavar "USERNAME",
                help "An admin"
              ]
          )
      )
    <*> many
      ( option
          (eitherReader (parseUsernameWithError . T.pack))
          ( mconcat
              [ long "freeloader",
                metavar "USERNAME",
                help "A user that can use the service for free"
              ]
          )
      )
    <*> parseMonetisationFlags
    <*> optional
      ( strOption
          ( mconcat
              [ long "verification-email-address",
                metavar "EMAIL_ADDRESS",
                help "The email address to use to send verification emails from"
              ]
          )
      )
    <*> optional
      ( strOption
          ( mconcat
              [ long "triggered-email-address",
                metavar "EMAIL_ADDRESS",
                help "The email address to use to send triggered item emails from"
              ]
          )
      )
    <*> optional
      ( strOption
          ( mconcat
              [ long "admin-notification-from-email-address",
                metavar "EMAIL_ADDRESS",
                help "The email address to use to send admin notification emails from"
              ]
          )
      )
    <*> optional
      ( strOption
          ( mconcat
              [ long "admin-notification-to-email-address",
                metavar "EMAIL_ADDRESS",
                help "The email address of the admin to use to send admin notification emails to"
              ]
          )
      )
    <*> getLooperFlags "triggerer"
    <*> getLooperFlags "emailer"
    <*> getLooperFlags "intray-item-scheduler"
    <*> getLooperFlags "intray-item-sender"
    <*> getLooperFlags "verification-email-converter"
    <*> getLooperFlags "triggered-email-scheduler"
    <*> getLooperFlags "triggered-email-converter"
    <*> getLooperFlags "admin-notification-email-converter"

parseMonetisationFlags :: Parser MonetisationFlags
parseMonetisationFlags =
  MonetisationFlags
    <$> optional
      ( strOption
          ( mconcat
              [ long "stripe-plan",
                metavar "PLAN_ID",
                help "The product pricing plan for stripe"
              ]
          )
      )
    <*> optional
      ( strOption
          ( mconcat
              [ long "stripe-secret-key",
                metavar "SECRET_KEY",
                help "The secret key for stripe"
              ]
          )
      )
    <*> optional
      ( strOption
          ( mconcat
              [ long "stripe-publishable-key",
                metavar "PUBLISHABLE_KEY",
                help "The publishable key for stripe"
              ]
          )
      )
    <*> getLooperFlags "stripe-events-fetcher"
    <*> optional
      ( option
          auto
          ( mconcat
              [ long "max-items-free",
                metavar "INT",
                help "How many items a user can sync in the free plan"
              ]
          )
      )
