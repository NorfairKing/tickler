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
    , CliM
    ) where

import Import

import qualified Data.ByteString as SB
import qualified Data.Text as T
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
                { registerSetUsername =
                      (T.pack <$> registerArgUsername) >>= parseUsername
                , registerSetPassword = T.pack <$> registerArgPassword
                }
        CommandLogin LoginArgs {..} ->
            pure $
            DispatchLogin
                LoginSettings
                { loginSetUsername =
                      (T.pack <$> loginArgUsername) >>= parseUsername
                , loginSetPassword = T.pack <$> loginArgPassword
                }
        CommandPostPostAddItem ss ->
            pure $ DispatchPostPostAddItem $ T.unwords $ map T.pack ss
        CommandShowItem -> pure DispatchShowItem
        CommandDoneItem -> pure DispatchDoneItem
        CommandSize -> pure DispatchSize
        CommandReview -> pure DispatchReview
        CommandLogout -> pure DispatchLogout
        CommandSync -> pure DispatchSync

getConfig :: Flags -> IO Configuration
getConfig Flags {..} = do
    path <- maybe (defaultConfigFile flagTicklerDir) resolveFile' flagConfigFile
    mContents <- forgivingAbsence $ SB.readFile $ fromAbsFile path
    case mContents of
        Nothing -> pure emptyConfiguration
        Just contents ->
            case Yaml.decodeEither contents of
                Left err ->
                    die $
                    unlines
                        [ "Failed to parse config file"
                        , fromAbsFile path
                        , "with error:"
                        , err
                        ]
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
        , command "add" parseCommandPostPostAddItem
        , command "show" parseCommandShowItem
        , command "done" parseCommandDoneItem
        , command "size" parseCommandSize
        , command "review" parseCommandReview
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
                  [ long "username"
                  , help "The username to register"
                  , value Nothing
                  , metavar "USERNAME"
                  ]) <*>
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
             (mconcat
                  [ long "username"
                  , help "The username to login"
                  , value Nothing
                  , metavar "USERNAME"
                  ]) <*>
         option
             (Just <$> str)
             (mconcat
                  [ long "password"
                  , help "The password to login with"
                  , value Nothing
                  , metavar "PASSWORD"
                  ]))

parseCommandPostPostAddItem :: ParserInfo Command
parseCommandPostPostAddItem = info parser modifier
  where
    modifier = fullDesc <> progDesc "Add an item"
    parser =
        CommandPostPostAddItem <$>
        some
            (strArgument
                 (mconcat
                      [ help "Give the contents of the item to be added."
                      , metavar "TEXT"
                      ]))

parseCommandShowItem :: ParserInfo Command
parseCommandShowItem = info parser modifier
  where
    modifier = fullDesc <> progDesc "Show one item."
    parser = pure CommandShowItem

parseCommandDoneItem :: ParserInfo Command
parseCommandDoneItem = info parser modifier
  where
    modifier = fullDesc <> progDesc "Mark that item as done."
    parser = pure CommandDoneItem

parseCommandSize :: ParserInfo Command
parseCommandSize = info parser modifier
  where
    modifier = fullDesc <> progDesc "Show the number of items in the tickler."
    parser = pure CommandSize

parseCommandReview :: ParserInfo Command
parseCommandReview = info parser modifier
  where
    modifier = fullDesc <> progDesc "Start reviewing items one by one."
    parser = pure CommandReview

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
        (mconcat
             [ long "url"
             , help "The url of the server."
             , value Nothing
             , metavar "URL"
             ]) <*>
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
    flag
        Nothing
        (Just NeverSync)
        (mconcat [long "no-sync", help "Do not try to sync."]) <|>
    flag
        Nothing
        (Just AlwaysSync)
        (mconcat [long "sync", help "Definitely try to sync."])
