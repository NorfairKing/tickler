{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Tickler.Cli.OptParse
  ( Instructions(..)
  , getInstructions
  , Settings(..)
  , SyncStrategy(..)
  , Dispatch(..)
  , RegisterSettings(..)
  , LoginSettings(..)
  , AddSettings(..)
  , CliM
  ) where

import Import

import qualified Data.ByteString as SB
import qualified Data.Text as T
import Data.Time
import Data.Yaml as Yaml (decodeEither)

import Options.Applicative
import System.Environment

import Servant.Client

import Tickler.Cli.OptParse.Types
import Tickler.Data

getInstructions :: IO Instructions
getInstructions = do
  Arguments cmd flg <- getArguments
  cfg <- getConfig flg
  dispatch <- getDispatch cmd
  settings <- getSettings cfg flg
  pure $ Instructions dispatch settings

getSettings :: Configuration -> Flags -> IO Settings
getSettings Configuration {..} Flags {..} = do
  setBaseUrl <-
    case flagUrl `mplus` configUrl of
      Nothing -> pure Nothing
      Just url -> Just <$> parseBaseUrl url
  setTicklerDir <-
    case flagTicklerDir `mplus` configTicklerDir of
      Nothing -> do
        home <- getHomeDir
        resolveDir home ".tickler"
      Just d -> resolveDir' d
  let setSyncStrategy =
        fromMaybe
          (case setBaseUrl of
             Nothing -> NeverSync
             Just _ -> AlwaysSync) $
        flagSyncStrategy `mplus` configSyncStrategy
  let setUsername = configUsername
  pure Settings {..}

getDispatch :: Command -> IO Dispatch
getDispatch cmd =
  case cmd of
    CommandRegister RegisterArgs {..} ->
      pure $
      DispatchRegister
        RegisterSettings
          { registerSetUsername = (T.pack <$> registerArgUsername) >>= parseUsername
          , registerSetPassword = T.pack <$> registerArgPassword
          }
    CommandLogin LoginArgs {..} ->
      pure $
      DispatchLogin
        LoginSettings
          { loginSetUsername = (T.pack <$> loginArgUsername) >>= parseUsername
          , loginSetPassword = T.pack <$> loginArgPassword
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
            { addSetTickleContent = T.pack addArgContent
            , addSetTickleDate = date
            , addSetTickleTime = mTime
            , addSetTickleRecurrence = r
            }
    CommandLogout -> pure DispatchLogout
    CommandSync -> pure DispatchSync

mkRecurrence :: RecurrenceArgs -> IO Recurrence
mkRecurrence (RecurrenceArgEveryDayAt mtod) =
  everyDaysAtTime 1 <$> mkTimeOfDay mtod >>= mkValidRecurrence
mkRecurrence (RecurrenceArgEveryDaysAt ds mtod) =
  everyDaysAtTime ds <$> mkTimeOfDay mtod >>= mkValidRecurrence
mkRecurrence (RecurrenceArgEveryMonthOnAt md mtod) =
  everyMonthsOnDayAtTime 1 md <$> mkTimeOfDay mtod >>= mkValidRecurrence
mkRecurrence (RecurrenceArgEveryMonthsOnAt ms md mtod) =
  everyMonthsOnDayAtTime ms md <$> mkTimeOfDay mtod >>= mkValidRecurrence

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

getConfig :: Flags -> IO Configuration
getConfig Flags {..} = do
  path <- maybe (defaultConfigFile flagTicklerDir) resolveFile' flagConfigFile
  mContents <- forgivingAbsence $ SB.readFile $ fromAbsFile path
  case mContents of
    Nothing -> pure emptyConfiguration
    Just contents ->
      case Yaml.decodeEither contents of
        Left err ->
          die $ unlines ["Failed to parse config file", fromAbsFile path, "with error:", err]
        Right conf -> pure conf

defaultConfigFile :: Maybe FilePath -> IO (Path Abs File)
defaultConfigFile mid = do
  i <-
    case mid of
      Nothing -> do
        homeDir <- getHomeDir
        resolveDir homeDir ".tickler"
      Just i -> resolveDir' i
  resolveFile i "config.yaml"

getArguments :: IO Arguments
getArguments = do
  args <- getArgs
  let result = runArgumentsParser args
  handleParseResult result

runArgumentsParser :: [String] -> ParserResult Arguments
runArgumentsParser = execParserPure prefs_ argParser

prefs_ :: ParserPrefs
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
    description = "tickler"

parseArgs :: Parser Arguments
parseArgs = Arguments <$> parseCommand <*> parseFlags

parseCommand :: Parser Command
parseCommand =
  hsubparser $
  mconcat
    [ command "register" parseCommandRegister
    , command "login" parseCommandLogin
    , command "add" parseCommandAdd
    , command "logout" parseCommandLogout
    , command "sync" parseCommandSync
    ]

parseCommandRegister :: ParserInfo Command
parseCommandRegister = info parser modifier
  where
    modifier = fullDesc <> progDesc "Register user"
    parser =
      CommandRegister <$>
      (RegisterArgs <$>
       option
         (Just <$> str)
         (mconcat
            [long "username", help "The username to register", value Nothing, metavar "USERNAME"]) <*>
       option
         (Just <$> str)
         (mconcat
            [ long "password"
            , help "The password to register with"
            , value Nothing
            , metavar "PASSWORD"
            ]))

parseCommandLogin :: ParserInfo Command
parseCommandLogin = info parser modifier
  where
    modifier = fullDesc <> progDesc "Login user"
    parser =
      CommandLogin <$>
      (LoginArgs <$>
       option
         (Just <$> str)
         (mconcat [long "username", help "The username to login", value Nothing, metavar "USERNAME"]) <*>
       option
         (Just <$> str)
         (mconcat
            [long "password", help "The password to login with", value Nothing, metavar "PASSWORD"]))

parseCommandAdd :: ParserInfo Command
parseCommandAdd = info parser modifier
  where
    modifier = fullDesc <> progDesc "Add a tickle"
    parser =
      CommandAdd <$>
      (AddArgs <$> strArgument (mconcat [metavar "CONTENT", help "The content of the tickle"]) <*>
       strArgument (mconcat [metavar "DATE", help "The scheduled date of the tickle"]) <*>
       option
         (Just <$> str)
         (mconcat
            [long "time", help "The scheduled time of the tickle", value Nothing, metavar "TIME"]) <*>
       optional parseRecurrence)

parseRecurrence :: Parser RecurrenceArgs
parseRecurrence = parseDaysAtTime <|> parseMonthsOnDay
  where
    parseDaysAtTime :: Parser RecurrenceArgs
    parseDaysAtTime = parseEveryDay <|> parseEveryXDays
    parseEveryDay :: Parser RecurrenceArgs
    parseEveryDay =
      flag' RecurrenceArgEveryDayAt (mconcat [long "every-day", help "Every day"]) <*>
      option
        (Just <$> str)
        (mconcat [long "at", help "at a given time", value Nothing, metavar "TIME"])
    parseEveryXDays :: Parser RecurrenceArgs
    parseEveryXDays =
      RecurrenceArgEveryDaysAt <$>
      option auto (mconcat [long "every-x-days", metavar "X", help "Every X days"]) <*>
      option
        (Just <$> str)
        (mconcat [long "at", help "at a given time", value Nothing, metavar "TIME"])
    parseMonthsOnDay :: Parser RecurrenceArgs
    parseMonthsOnDay = parseEveryMonthOnAt <|> parseEveryXMonthsOnAt
    parseEveryMonthOnAt :: Parser RecurrenceArgs
    parseEveryMonthOnAt =
      flag' RecurrenceArgEveryMonthOnAt (mconcat [long "every-month", help "Every month"]) <*>
      option
        (Just <$> auto)
        (mconcat [long "on", help "on a given day of the month", value Nothing, metavar "DAY"]) <*>
      option
        (Just <$> str)
        (mconcat [long "at", help "at a given time", value Nothing, metavar "TIME"])
    parseEveryXMonthsOnAt :: Parser RecurrenceArgs
    parseEveryXMonthsOnAt =
      RecurrenceArgEveryMonthsOnAt <$>
      option auto (mconcat [long "every-x-months", metavar "X", help "Every X months"]) <*>
      option
        (Just <$> auto)
        (mconcat [long "on", help "on a given day of the month", value Nothing, metavar "DAY"]) <*>
      option
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
  Flags <$>
  option
    (Just <$> str)
    (mconcat
       [ long "config-file"
       , help "Give the path to an altenative config file"
       , value Nothing
       , metavar "FILEPATH"
       ]) <*>
  option
    (Just <$> str)
    (mconcat [long "url", help "The url of the server.", value Nothing, metavar "URL"]) <*>
  option
    (Just <$> str)
    (mconcat
       [ long "tickler-dir"
       , help "The directory to use for caching and state"
       , value Nothing
       , metavar "URL"
       ]) <*>
  syncStrategyOpt

syncStrategyOpt :: Parser (Maybe SyncStrategy)
syncStrategyOpt =
  flag Nothing (Just NeverSync) (mconcat [long "no-sync", help "Do not try to sync."]) <|>
  flag Nothing (Just AlwaysSync) (mconcat [long "sync", help "Definitely try to sync."])
