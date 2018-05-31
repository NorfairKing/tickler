{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

module Tickler.Server.OptParse
    ( module Tickler.Server.OptParse
    , module Tickler.Server.OptParse.Types
    ) where

import Import

import qualified Data.Text as T
import Text.Read

import System.Environment (getArgs, getEnvironment)

import Control.Monad.Trans.AWS as AWS
import Database.Persist.Sqlite
import Servant.Client.Core

import Options.Applicative

import Tickler.API
import Tickler.Server.OptParse.Types

getInstructions :: IO Instructions
getInstructions = do
    Arguments cmd flags <- getArguments
    config <- getConfiguration cmd flags
    env <- getEnv
    combineToInstructions cmd flags config env

combineToInstructions ::
       Command -> Flags -> Configuration -> Environment -> IO Instructions
combineToInstructions (CommandServe ServeFlags {..}) Flags Configuration Environment {..} = do
    let serveSetPort = fromMaybe 8001 $ serveFlagPort `mplus` envPort
    let serveSetConnectionInfo =
            mkSqliteConnectionInfo $ fromMaybe "tickler.db" serveFlagDb
    let serveSetConnectionCount = fromMaybe 4 serveFlagConnectionCount
    serveSetAdmins <-
        forM serveFlagAdmins $ \s ->
            case parseUsername $ T.pack s of
                Nothing -> die $ unwords ["Invalid admin username:", s]
                Just u -> pure u
    let LooperFlags {..} = serveFlagsLooperFlags
    let defaultLoopersEnabled = fromMaybe True looperFlagDefaultEnabled
    let defaultLoopersPeriod = fromMaybe 60 looperFlagDefaultPeriod
    let looperSetConnectionInfo = serveSetConnectionInfo
    let looperSetConnectionCount = serveSetConnectionCount
    let combineToLooperSets ::
               LooperFlagsWith a -> (a -> IO b) -> IO (LooperSetsWith b)
        combineToLooperSets LooperFlagsWith {..} func = do
            let enabled = fromMaybe defaultLoopersEnabled looperFlagEnable
            if enabled
                then do
                    let period =
                            fromMaybe defaultLoopersPeriod looperFlagsPeriod
                    LooperEnabled period <$> func looperFlags
                else pure LooperDisabled
    looperSetTriggererSets <-
        combineToLooperSets looperFlagTriggererFlags $
        const $ pure TriggererSettings
    looperSetEmailerSets <-
        combineToLooperSets looperFlagEmailerFlags $
        const $ pure $ EmailerSettings Discover
    looperSetTriggeredIntrayItemSchedulerSets <-
        combineToLooperSets looperFlagTriggeredIntrayItemSchedulerFlags $
        const $ pure ()
    looperSetTriggeredIntrayItemSenderSets <-
        combineToLooperSets looperFlagTriggeredIntrayItemSenderFlags $
        const $ pure ()
    looperSetVerificationEmailConverterSets <-
        combineToLooperSets looperFlagVerificationEmailConverterFlags $
        const $ pure ()
    looperSetTriggeredEmailSchedulerSets <-
        combineToLooperSets looperFlagTriggeredEmailSchedulerFlags $
        const $ pure ()
    looperSetTriggeredEmailConverterSets <-
        combineToLooperSets looperFlagTriggeredEmailConverterFlags $ \() -> do
            pure
                TriggeredEmailConverterSettings
                { triggeredEmailConverterSetFromAddress =
                      unsafeEmailAddress "tickler" "cs-syd.eu"
                , triggeredEmailConverterSetFromName = "Tickler"
                }
    let serveSetLooperSettings = LooperSettings {..}
    pure $ Instructions (DispatchServe ServeSettings {..}) Settings

getConfiguration :: Command -> Flags -> IO Configuration
getConfiguration _ _ = pure Configuration

getEnv :: IO Environment
getEnv = do
    env <- getEnvironment
    let mre k func =
            forM (lookup k env) $ \s ->
                case func s of
                    Left e ->
                        die $
                        unwords
                            [ "Unable to read ENV Var:"
                            , k
                            , "which has value:"
                            , show s
                            , "with error:"
                            , e
                            ]
                    Right v -> pure v
        mrf k func =
            mre k $ \s ->
                case func s of
                    Nothing ->
                        Left "Parsing failed without a good error message."
                    Just v -> Right v
        mr k = mrf k readMaybe
    envPort <- mr "API_PORT"
    pure Environment {..}

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
             [ long "api-port"
             , value Nothing
             , metavar "PORT"
             , help "the port to serve the API on"
             ]) <*>
    option
        (Just . T.pack <$> str)
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
    many
        (strOption
             (mconcat [long "admin", metavar "USERNAME", help "An admin to use"])) <*>
    parseLooperFlags

parseLooperFlags :: Parser LooperFlags
parseLooperFlags =
    LooperFlags <$>
    onOffFlag
        "loopers"
        (help $ unwords ["enable or disable all the loopers by default"]) <*>
    option
        (Just <$> auto)
        (mconcat
             [ long "default-period"
             , value Nothing
             , metavar "SECONDS"
             , help "The default period for all loopers"
             ]) <*>
    parseLooperFlagsWith "triggerer" (pure ()) <*>
    parseLooperFlagsWith "emailer" (pure ()) <*>
    parseLooperFlagsWith "intray-item-scheduler" (pure ()) <*>
    parseLooperFlagsWith "intray-item-sender" (pure ()) <*>
    parseLooperFlagsWith "verification-email-converter" (pure ()) <*>
    parseLooperFlagsWith "triggered-email-scheduler" (pure ()) <*>
    parseLooperFlagsWith "triggered-email-converter" (pure ())

parseLooperFlagsWith :: String -> Parser a -> Parser (LooperFlagsWith a)
parseLooperFlagsWith name func =
    LooperFlagsWith <$>
    onOffFlag
        (intercalate "-" [name, "looper"])
        (mconcat [help $ unwords ["enable or disable the", name, "looper"]]) <*>
    option
        (Just <$> auto)
        (mconcat
             [ long $ intercalate "-" [name, "period"]
             , value Nothing
             , metavar "SECONDS"
             , help $ unwords ["The period for", name]
             ]) <*>
    func

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
