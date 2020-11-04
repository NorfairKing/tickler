{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}

module Tickler.Cli.OptParse
  ( Instructions (..),
    getInstructions,
    Settings (..),
    SyncStrategy (..),
    Dispatch (..),
    RegisterSettings (..),
    LoginSettings (..),
    AddSettings (..),
    CliM,
  )
where

import qualified Data.Text as T
import Data.Time
import Import
import Options.Applicative
import Servant.Client
import qualified System.Environment as System (getArgs, getEnvironment)
import Text.Read (readMaybe)
import Tickler.Cli.OptParse.Types
import Tickler.Data
import YamlParse.Applicative (confDesc, readConfigFile, readFirstConfigFile)

getInstructions :: IO Instructions
getInstructions = do
  args@(Arguments _ flags) <- getArguments
  environment <- getEnvironment
  configuration <- getConfiguration flags environment
  combineToInstructions args environment configuration

combineToInstructions :: Arguments -> Environment -> Maybe Configuration -> IO Instructions
combineToInstructions (Arguments cmd Flags {..}) Environment {..} mConf =
  Instructions <$> getDispatch <*> getSettings
  where
    mc :: (Configuration -> Maybe a) -> Maybe a
    mc func = mConf >>= func
    getSettings = do
      setBaseUrl <-
        case flagUrl <|> envUrl <|> mc configUrl of
          Nothing -> pure Nothing
          Just url -> Just <$> parseBaseUrl url
      setCacheDir <-
        case flagCacheDir <|> envCacheDir <|> mc configCacheDir of
          Nothing -> getXdgDir XdgCache (Just [reldir|tickler|])
          Just d -> resolveDir' d
      setDataDir <-
        case flagDataDir <|> envDataDir <|> mc configDataDir of
          Nothing -> getXdgDir XdgData (Just [reldir|tickler|])
          Just d -> resolveDir' d
      let setSyncStrategy =
            fromMaybe
              ( case setBaseUrl of
                  Nothing -> NeverSync
                  Just _ -> AlwaysSync
              )
              $ flagSyncStrategy <|> envSyncStrategy <|> mc configSyncStrategy
      setUsername <-
        case envUsername <|> mc configUsername of
          Nothing -> pure Nothing
          Just us ->
            case parseUsername (T.pack us) of
              Nothing -> die $ "Invalid username: " <> us
              Just un -> pure $ Just un
      pure Settings {..}
    getDispatch =
      case cmd of
        CommandRegister RegisterArgs {..} ->
          pure $
            DispatchRegister
              RegisterSettings
                { registerSetUsername =
                    (T.pack <$> (registerArgUsername <|> envUsername <|> mc configUsername))
                      >>= parseUsername,
                  registerSetPassword =
                    T.pack <$> (registerArgPassword <|> envPassword <|> mc configPassword)
                }
        CommandLogin LoginArgs {..} ->
          pure $
            DispatchLogin
              LoginSettings
                { loginSetUsername =
                    (T.pack <$> (loginArgUsername <|> envUsername <|> mc configUsername))
                      >>= parseUsername,
                  loginSetPassword =
                    T.pack <$> (loginArgPassword <|> envPassword <|> mc configPassword)
                }
        CommandAdd AddArgs {..} -> do
          date <- parseTimeM True defaultTimeLocale "%Y-%-m-%-d" addArgTickleDate
          mTime <- mkTimeOfDay addArgTickleTime
          r <-
            case addArgRecurrence of
              Nothing -> pure Nothing
              Just ras -> Just <$> mkRecurrence ras
          pure $
            DispatchAdd
              AddSettings
                { addSetTickleContent = T.pack addArgContent,
                  addSetTickleDate = date,
                  addSetTickleTime = mTime,
                  addSetTickleRecurrence = r
                }
        CommandLogout -> pure DispatchLogout
        CommandSync -> pure DispatchSync

mkRecurrence :: RecurrenceArgs -> IO Recurrence
mkRecurrence ra =
  (mkValidRecurrence =<<) $
    case ra of
      RecurrenceArgEveryDayAt mtod -> everyDaysAtTime 1 <$> mkTimeOfDay mtod
      RecurrenceArgEveryDaysAt ds mtod -> everyDaysAtTime ds <$> mkTimeOfDay mtod
      RecurrenceArgEveryMonthOnAt md mtod -> everyMonthsOnDayAtTime 1 md <$> mkTimeOfDay mtod
      RecurrenceArgEveryMonthsOnAt ms md mtod -> everyMonthsOnDayAtTime ms md <$> mkTimeOfDay mtod

mkTimeOfDay :: Maybe String -> IO (Maybe TimeOfDay)
mkTimeOfDay Nothing = pure Nothing
mkTimeOfDay (Just a) =
  fmap Just $
    case parseTimeM True defaultTimeLocale "%H:%M" a of
      Nothing -> die $ unwords ["Invalid time of day:", a]
      Just tod -> pure tod

mkValidRecurrence :: Maybe Recurrence -> IO Recurrence
mkValidRecurrence Nothing = die "Recurrence invalid:"
mkValidRecurrence (Just r) = pure r

getEnvironment :: IO Environment
getEnvironment = do
  env <- System.getEnvironment
  let ms key = lookup ("TICKLER_" <> key) env
      mr :: Read a => String -> IO (Maybe a)
      mr key =
        case ms key of
          Nothing -> pure Nothing
          Just s ->
            case readMaybe s of
              Nothing -> die $ "Un-read-able value: " <> s
              Just r -> pure $ Just r
  let envConfigFile = ms "CONFIG_FILE"
      envUrl = ms "URL"
      envCacheDir = ms "CACHE_DIR"
      envDataDir = ms "DATA_DIR"
  envSyncStrategy <- mr "SYNC_STRATEGY"
  let envUsername = ms "USERNAME"
  let envPassword = ms "PASSWORD"
  pure Environment {..}

getConfiguration :: Flags -> Environment -> IO (Maybe Configuration)
getConfiguration Flags {..} Environment {..} =
  case flagConfigFile <|> envConfigFile of
    Nothing -> defaultConfigFiles >>= readFirstConfigFile
    Just cf -> resolveFile' cf >>= readConfigFile

defaultConfigFiles :: IO [Path Abs File]
defaultConfigFiles =
  sequence
    [ do
        xdgConfigDir <- getXdgDir XdgConfig (Just [reldir|tickler|])
        resolveFile xdgConfigDir "config.yaml",
      do
        homeDir <- getHomeDir
        ticklerDir <- resolveDir homeDir ".tickler"
        resolveFile ticklerDir "config.yaml"
    ]

getArguments :: IO Arguments
getArguments = do
  args <- System.getArgs
  let result = runArgumentsParser args
  handleParseResult result

runArgumentsParser :: [String] -> ParserResult Arguments
runArgumentsParser = execParserPure prefs_ argParser

prefs_ :: ParserPrefs
prefs_ = defaultPrefs {prefShowHelpOnError = True, prefShowHelpOnEmpty = True}

argParser :: ParserInfo Arguments
argParser = info (helper <*> parseArgs) help_
  where
    help_ = fullDesc <> progDesc description <> confDesc @Configuration
    description = "tickler"

parseArgs :: Parser Arguments
parseArgs = Arguments <$> parseCommand <*> parseFlags

parseCommand :: Parser Command
parseCommand =
  hsubparser $
    mconcat
      [ command "register" parseCommandRegister,
        command "login" parseCommandLogin,
        command "add" parseCommandAdd,
        command "logout" parseCommandLogout,
        command "sync" parseCommandSync
      ]

parseCommandRegister :: ParserInfo Command
parseCommandRegister = info parser modifier
  where
    modifier = fullDesc <> progDesc "Register user"
    parser =
      CommandRegister
        <$> ( RegisterArgs
                <$> option
                  (Just <$> str)
                  ( mconcat
                      [long "username", help "The username to register", value Nothing, metavar "USERNAME"]
                  )
                <*> option
                  (Just <$> str)
                  ( mconcat
                      [ long "password",
                        help "The password to register with",
                        value Nothing,
                        metavar "PASSWORD"
                      ]
                  )
            )

parseCommandLogin :: ParserInfo Command
parseCommandLogin = info parser modifier
  where
    modifier = fullDesc <> progDesc "Login user"
    parser =
      CommandLogin
        <$> ( LoginArgs
                <$> option
                  (Just <$> str)
                  (mconcat [long "username", help "The username to login", value Nothing, metavar "USERNAME"])
                <*> option
                  (Just <$> str)
                  ( mconcat
                      [long "password", help "The password to login with", value Nothing, metavar "PASSWORD"]
                  )
            )

parseCommandAdd :: ParserInfo Command
parseCommandAdd = info parser modifier
  where
    modifier = fullDesc <> progDesc "Add a tickle"
    parser =
      CommandAdd
        <$> ( AddArgs <$> strArgument (mconcat [metavar "CONTENT", help "The content of the tickle"])
                <*> strArgument (mconcat [metavar "DATE", help "The scheduled date of the tickle"])
                <*> option
                  (Just <$> str)
                  ( mconcat
                      [long "time", help "The scheduled time of the tickle", value Nothing, metavar "TIME"]
                  )
                <*> optional parseRecurrence
            )

parseRecurrence :: Parser RecurrenceArgs
parseRecurrence = parseDaysAtTime <|> parseMonthsOnDay
  where
    parseDaysAtTime :: Parser RecurrenceArgs
    parseDaysAtTime = parseEveryDay <|> parseEveryXDays
    parseEveryDay :: Parser RecurrenceArgs
    parseEveryDay =
      flag' RecurrenceArgEveryDayAt (mconcat [long "every-day", help "Every day"])
        <*> option
          (Just <$> str)
          (mconcat [long "at", help "at a given time", value Nothing, metavar "TIME"])
    parseEveryXDays :: Parser RecurrenceArgs
    parseEveryXDays =
      RecurrenceArgEveryDaysAt
        <$> option auto (mconcat [long "every-x-days", metavar "X", help "Every X days"])
        <*> option
          (Just <$> str)
          (mconcat [long "at", help "at a given time", value Nothing, metavar "TIME"])
    parseMonthsOnDay :: Parser RecurrenceArgs
    parseMonthsOnDay = parseEveryMonthOnAt <|> parseEveryXMonthsOnAt
    parseEveryMonthOnAt :: Parser RecurrenceArgs
    parseEveryMonthOnAt =
      flag' RecurrenceArgEveryMonthOnAt (mconcat [long "every-month", help "Every month"])
        <*> option
          (Just <$> auto)
          (mconcat [long "on", help "on a given day of the month", value Nothing, metavar "DAY"])
        <*> option
          (Just <$> str)
          (mconcat [long "at", help "at a given time", value Nothing, metavar "TIME"])
    parseEveryXMonthsOnAt :: Parser RecurrenceArgs
    parseEveryXMonthsOnAt =
      RecurrenceArgEveryMonthsOnAt
        <$> option auto (mconcat [long "every-x-months", metavar "X", help "Every X months"])
        <*> option
          (Just <$> auto)
          (mconcat [long "on", help "on a given day of the month", value Nothing, metavar "DAY"])
        <*> option
          (Just <$> str)
          (mconcat [long "at", help "at a given time", value Nothing, metavar "TIME"])

parseCommandLogout :: ParserInfo Command
parseCommandLogout = info parser modifier
  where
    modifier = fullDesc <> progDesc "Logout user"
    parser = pure CommandLogout

parseCommandSync :: ParserInfo Command
parseCommandSync = info parser modifier
  where
    modifier = fullDesc <> progDesc "Sync the local and remote tickler"
    parser = pure CommandSync

parseFlags :: Parser Flags
parseFlags =
  Flags
    <$> option
      (Just <$> str)
      ( mconcat
          [ long "config-file",
            help "Give the path to an altenative config file",
            value Nothing,
            metavar "FILEPATH"
          ]
      )
    <*> option
      (Just <$> str)
      (mconcat [long "url", help "The url of the server.", value Nothing, metavar "URL"])
    <*> option
      (Just <$> str)
      ( mconcat
          [long "cache-dir", help "The directory to use for caching", value Nothing, metavar "DIR"]
      )
    <*> option
      (Just <$> str)
      (mconcat [long "data-dir", help "The directory to use for state", value Nothing, metavar "DIR"])
    <*> syncStrategyOpt

syncStrategyOpt :: Parser (Maybe SyncStrategy)
syncStrategyOpt =
  flag Nothing (Just NeverSync) (mconcat [long "no-sync", help "Do not try to sync."])
    <|> flag Nothing (Just AlwaysSync) (mconcat [long "sync", help "Definitely try to sync."])
