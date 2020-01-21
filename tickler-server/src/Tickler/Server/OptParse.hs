{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

module Tickler.Server.OptParse
  ( module Tickler.Server.OptParse
  , module Tickler.Server.OptParse.Types
  ) where

import Import

import qualified Data.Text as T
import Text.Read

import Looper

import System.Environment (getArgs, getEnvironment, lookupEnv)

import qualified Control.Monad.Trans.AWS as AWS
import Database.Persist.Sqlite

import Options.Applicative

import Tickler.API
import Tickler.Server.OptParse.Types

getInstructions :: IO Instructions
getInstructions = do
  Arguments cmd flags <- getArguments
  config <- getConfiguration cmd flags
  env <- getEnv
  combineToInstructions cmd flags config env

combineToInstructions :: Command -> Flags -> Configuration -> Environment -> IO Instructions
combineToInstructions (CommandServe ServeFlags {..}) Flags Configuration Environment {..} = do
  let serveSetPort = fromMaybe 8001 $ serveFlagPort `mplus` envPort
  webHost <-
    case serveFlagWebHost `mplus` envWebHost of
      Nothing -> die "No web host configured."
      Just wh -> pure $ T.pack wh
  dbPath <- resolveFile' $ fromMaybe "tickler.db" $ serveFlagDb <> envDb
  let serveSetConnectionInfo = mkSqliteConnectionInfo $ T.pack $ fromAbsFile dbPath
  let serveSetConnectionCount = fromMaybe 4 serveFlagConnectionCount
  serveSetAdmins <-
    forM serveFlagAdmins $ \s ->
      case parseUsername $ T.pack s of
        Nothing -> die $ unwords ["Invalid admin username:", s]
        Just u -> pure u
  let LoopersFlags {..} = serveFlagsLooperFlags
  let LoopersEnvironment {..} = envLoopersEnvironment
  let defaultLoopersEnabled =
        fromMaybe True $ looperFlagDefaultEnabled `mplus` looperEnvDefaultEnabled
  let defaultLoopersPeriod =
        fromIntegral $ fromMaybe 60 $ looperFlagDefaultPeriod `mplus` looperEnvDefaultPeriod
  let defaultLooperRetryDelay =
        fromMaybe 1000000 $ looperFlagDefaultRetryDelay `mplus` looperEnvDefaultRetryDelay
  let defaultLooperRetryAmount =
        fromMaybe 7 $ looperFlagDefaultRetryTimes `mplus` looperEnvDefaultRetryTimes
  let looperSetConnectionInfo = serveSetConnectionInfo
  let looperSetConnectionCount = serveSetConnectionCount
  let combineToLooperSets :: LooperFlags -> LooperEnvironment -> IO c -> IO (LooperSetsWith c)
      combineToLooperSets lflags lenv func = do
        let sets = deriveLooperSettings (seconds 0) defaultLoopersPeriod lflags lenv Nothing
        if looperSetEnabled sets
          then LooperEnabled sets <$> func
          else pure LooperDisabled
  looperSetTriggererSets <-
    combineToLooperSets looperFlagTriggererFlags looperEnvTriggererEnv $ pure TriggererSettings
  looperSetEmailerSets <-
    combineToLooperSets looperFlagEmailerFlags looperEnvEmailerEnv $
    pure $ EmailerSettings AWS.Discover
  looperSetTriggeredIntrayItemSchedulerSets <-
    combineToLooperSets
      looperFlagTriggeredIntrayItemSchedulerFlags
      looperEnvTriggeredIntrayItemSchedulerEnv $
    pure ()
  looperSetTriggeredIntrayItemSenderSets <-
    combineToLooperSets
      looperFlagTriggeredIntrayItemSenderFlags
      looperEnvTriggeredIntrayItemSenderEnv $
    pure ()
  looperSetVerificationEmailConverterSets <-
    combineToLooperSets
      looperFlagVerificationEmailConverterFlags
      looperEnvVerificationEmailConverterEnv $
    pure
      VerificationEmailConverterSettings
        { verificationEmailConverterSetFromAddress -- TODO make these configurable
           = unsafeEmailAddress "verification" "tickler.cs-syd.eu"
        , verificationEmailConverterSetFromName = "Tickler"
        , verificationEmailConverterSetWebHost = webHost
        }
  looperSetTriggeredEmailSchedulerSets <-
    combineToLooperSets looperFlagTriggeredEmailSchedulerFlags looperEnvTriggeredEmailSchedulerEnv $
    pure ()
  looperSetTriggeredEmailConverterSets <-
    combineToLooperSets looperFlagTriggeredEmailConverterFlags looperEnvTriggeredEmailConverterEnv $
    pure
      TriggeredEmailConverterSettings
        { triggeredEmailConverterSetFromAddress -- TODO make these configurable
           = unsafeEmailAddress "triggered" "tickler.cs-syd.eu"
        , triggeredEmailConverterSetFromName = "Tickler"
        , triggeredEmailConverterSetWebHost = webHost
        }
  let serveSetLooperSettings = LoopersSettings {..}
  pure $ Instructions (DispatchServe ServeSettings {..}) Settings

getConfiguration :: Command -> Flags -> IO Configuration
getConfiguration _ _ = pure Configuration

getEnv :: IO Environment
getEnv = do
  env <- getEnvironment
  envDb <- lookupEnv "DATABASE"
  envWebHost <- lookupEnv "WEB_HOST"
  envPort <- maybeReadEnv "API_PORT" env
  envLoopersEnvironment <- getLoopersEnv
  pure Environment {..}

getLoopersEnv :: IO LoopersEnvironment
getLoopersEnv = do
  env <- getEnvironment
  let le n = readLooperEnvironment "LOOPER_" n env
  looperEnvDefaultEnabled <- maybeReadEnv "LOOPERS_DEFAULT_ENABLED" env
  looperEnvDefaultPeriod <- maybeReadEnv "LOOPERS_DEFAULT_PERIOD" env
  looperEnvDefaultRetryDelay <- maybeReadEnv "LOOPERS_DEFAULT_RETRY_DELAY" env
  looperEnvDefaultRetryTimes <- maybeReadEnv "LOOPERS_DEFAULT_RETRY_AMOUNT" env
  let looperEnvTriggererEnv = le "TRIGGERER"
      looperEnvEmailerEnv = le "EMAILER"
      looperEnvTriggeredIntrayItemSchedulerEnv = le "TRIGGERED_INTRAY_ITEM_SCHEDULER"
      looperEnvTriggeredIntrayItemSenderEnv = le "TRIGGERED_INTRAY_ITEM_SENDER"
      looperEnvVerificationEmailConverterEnv = le "VERIFICATION_EMAIL_CONVERTER"
      looperEnvTriggeredEmailSchedulerEnv = le "TRIGGERED_EMAIL_SCHEDULER"
      looperEnvTriggeredEmailConverterEnv = le "TRIGGERED_EMAIL_CONVERTER"
  pure LoopersEnvironment {..}

eitherParseEnv :: Show a => String -> (a -> Either String b) -> [(String, a)] -> IO (Maybe b)
eitherParseEnv k func env =
  forM (lookup k env) $ \s ->
    case func s of
      Left e ->
        die $ unwords ["Unable to read ENV Var:", k, "which has value:", show s, "with error:", e]
      Right v -> pure v

maybeParseEnv :: Show a => String -> (a -> Maybe b) -> [(String, a)] -> IO (Maybe b)
maybeParseEnv k func =
  eitherParseEnv k $ \s ->
    case func s of
      Nothing -> Left "Parsing failed without a good error message."
      Just v -> Right v

maybeReadEnv :: Read b => String -> [(String, String)] -> IO (Maybe b)
maybeReadEnv k = maybeParseEnv k readMaybe

getArguments :: IO Arguments
getArguments = do
  args <- getArgs
  let result = runArgumentsParser args
  handleParseResult result

runArgumentsParser :: [String] -> ParserResult Arguments
runArgumentsParser = execParserPure prefs_ argParser
  where
    prefs_ =
      ParserPrefs
        { prefMultiSuffix = ""
        , prefDisambiguate = True
        , prefShowHelpOnError = True
        , prefShowHelpOnEmpty = True
        , prefBacktrack = True
        , prefColumns = 80
        }

argParser :: ParserInfo Arguments
argParser = info (helper <*> parseArgs) help_
  where
    help_ = fullDesc <> progDesc description
    description = "Tickler server"

parseArgs :: Parser Arguments
parseArgs = Arguments <$> parseCommand <*> parseFlags

parseCommand :: Parser Command
parseCommand = hsubparser $ mconcat [command "serve" parseCommandServe]

parseCommandServe :: ParserInfo Command
parseCommandServe = info parser modifier
  where
    parser = CommandServe <$> parseServeFlags
    modifier = fullDesc <> progDesc "Command example."

parseServeFlags :: Parser ServeFlags
parseServeFlags =
  ServeFlags <$>
  option
    (Just <$> auto)
    (mconcat
       [long "web-host", value Nothing, metavar "HOST", help "the host to serve the web server on"]) <*>
  option
    (Just <$> auto)
    (mconcat [long "api-port", value Nothing, metavar "PORT", help "the port to serve the API on"]) <*>
  option
    (Just <$> str)
    (mconcat
       [ long "database"
       , value Nothing
       , metavar "DATABASE_CONNECTION_STRING"
       , help "The sqlite connection string"
       ]) <*>
  option
    (Just <$> auto)
    (mconcat
       [ long "connection-count"
       , value Nothing
       , metavar "CONNECTION_COUNT"
       , help "the number of database connections to use"
       ]) <*>
  many (strOption (mconcat [long "admin", metavar "USERNAME", help "An admin to use"])) <*>
  parseLooperFlags

parseLooperFlags :: Parser LoopersFlags
parseLooperFlags =
  LoopersFlags <$>
  onOffFlag "loopers" (help $ unwords ["enable or disable all the loopers by default"]) <*>
  option
    (Just <$> auto)
    (mconcat
       [ long "default-period"
       , value Nothing
       , metavar "SECONDS"
       , help "The default period for all loopers"
       ]) <*>
  option
    (Just <$> auto)
    (mconcat
       [ long "default-retry-delay"
       , value Nothing
       , metavar "MICROSECONDS"
       , help "The retry delay for all loopers, in microseconds"
       ]) <*>
  option
    (Just <$> auto)
    (mconcat
       [ long "default-retry-amount"
       , value Nothing
       , metavar "AMOUNT"
       , help "The default amount of times to retry a looper before failing"
       ]) <*>
  getLooperFlags "triggerer" <*>
  getLooperFlags "emailer" <*>
  getLooperFlags "intray-item-scheduler" <*>
  getLooperFlags "intray-item-sender" <*>
  getLooperFlags "verification-email-converter" <*>
  getLooperFlags "triggered-email-scheduler" <*>
  getLooperFlags "triggered-email-converter"

onOffFlag :: String -> Mod FlagFields (Maybe Bool) -> Parser (Maybe Bool)
onOffFlag suffix mods =
  flag' (Just True) (mconcat [long $ pf "enable", hidden]) <|>
  flag' (Just False) (mconcat [long $ pf "disable", hidden]) <|>
  flag' Nothing (mconcat [long ("(enable|disable)-" ++ suffix), mods]) <|>
  pure Nothing
  where
    pf s = intercalate "-" [s, suffix]

parseFlags :: Parser Flags
parseFlags = pure Flags
