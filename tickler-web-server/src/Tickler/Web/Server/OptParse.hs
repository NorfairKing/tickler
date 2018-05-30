{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

module Tickler.Web.Server.OptParse
    ( getInstructions
    , Instructions
    , Dispatch(..)
    , Settings(..)
    , ServeSettings(..)
    ) where

import Import

import qualified Data.Text as T
import Servant.Client.Core
import System.Environment (getArgs, getEnvironment)
import Text.Read

import Database.Persist.Sqlite

import Options.Applicative

import Tickler.API
import Tickler.Web.Server.OptParse.Types

getInstructions :: IO Instructions
getInstructions = do
    (cmd, flags) <- getArguments
    config <- getConfiguration cmd flags
    env <- getEnv
    combineToInstructions cmd flags config env

combineToInstructions ::
       Command -> Flags -> Configuration -> Environment -> IO Instructions
combineToInstructions (CommandServe ServeFlags {..}) Flags Configuration Environment {..} = do
    let port = fromMaybe 8000 $ serveFlagPort `mplus` envPort
    let apiPort = fromMaybe 8001 $ serveFlagAPIPort `mplus` envAPIPort
    let connInfo =
            mkSqliteConnectionInfo $ fromMaybe "tickler.db" serveFlagAPIDB
    let connCount = fromMaybe 4 serveFlagAPIConnectionCount
    when (apiPort == port) $
        die $
        unlines
            [ "Web server port and API port must not be the same."
            , "They are both: " ++ show port
            ]
    admins <-
        forM serveFlagAPIAdmins $ \s ->
            case parseUsername $ T.pack s of
                Nothing -> die $ unwords ["Invalid admin username:", s]
                Just u -> pure u
    pure
        ( DispatchServe
              ServeSettings
              { serveSetPort = port
              , serveSetPersistLogins = fromMaybe False serveFlagPersistLogins
              , serveSetDefaultIntrayUrl = serveFlagDefaultIntrayUrl `mplus` envDefaultIntrayUrl
              , serveSetAPIPort = apiPort
              , serveSetAPIConnectionInfo = connInfo
              , serveSetAPIConnectionCount = connCount
              , serveSetAPIAdmins = admins
              }
        , Settings)

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
    envPort <- mr "PORT"
    envAPIPort <- mr "API_PORT"
    envDefaultIntrayUrl <- mre "DEFAULT_INTRAY_URL" (left show . parseBaseUrl)
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
    description = "Tickler web server"

parseArgs :: Parser Arguments
parseArgs = (,) <$> parseCommand <*> parseFlags

parseCommand :: Parser Command
parseCommand = hsubparser $ mconcat [command "serve" parseCommandServe]

parseCommandServe :: ParserInfo Command
parseCommandServe = info parser modifier
  where
    parser =
        CommandServe <$>
        (ServeFlags <$>
         option
             (Just <$> auto)
             (mconcat
                  [ long "port"
                  , metavar "PORT"
                  , value Nothing
                  , help "the port to serve on"
                  ]) <*>
         flag
             Nothing
             (Just True)
             (mconcat
                  [ long "persist-logins"
                  , help
                        "Whether to persist logins accross restarts. This should not be used in production."
                  ]) <*>
         option
             (Just <$> eitherReader (left show . parseBaseUrl))
             (mconcat
                  [ long "default-intray-url"
                  , value Nothing
                  , help
                        "The default intray url to suggest when adding an intray trigger."
                  ]) <*>
         option
             (Just <$> auto)
             (mconcat
                  [ long "api-port"
                  , value Nothing
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
                  (mconcat
                       [ long "admin"
                       , metavar "USERNAME"
                       , help "An admin to use"
                       ])))
    modifier = fullDesc <> progDesc "Serve."

parseFlags :: Parser Flags
parseFlags = pure Flags
