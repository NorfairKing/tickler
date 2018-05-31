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

import Servant.Client.Core
import System.Environment (getArgs, getEnvironment)
import Text.Read


import Options.Applicative

import qualified Tickler.Server.OptParse as API

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
    API.Instructions (API.DispatchServe apiServeSets) API.Settings <-
        API.combineToInstructions
            (API.CommandServe serveFlagAPIServeFlags)
            API.Flags
            API.Configuration
            envAPIEnvironment
    let webPort = fromMaybe 8000 $ serveFlagPort `mplus` envPort
    when (API.serveSetPort apiServeSets == webPort) $
        die $
        unlines
            [ "Web server port and API port must not be the same."
            , "They are both: " ++ show webPort
            ]
    pure
        ( DispatchServe
              ServeSettings
              { serveSetPort = webPort
              , serveSetPersistLogins = fromMaybe False serveFlagPersistLogins
              , serveSetDefaultIntrayUrl =
                    serveFlagDefaultIntrayUrl `mplus` envDefaultIntrayUrl
              , serveSetAPISettings = apiServeSets
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
    envPort <- mr "WEB_PORT"
    envDefaultIntrayUrl <-
        mre "WEB_DEFAULT_INTRAY_URL" (left show . parseBaseUrl)
    envAPIEnvironment <- API.getEnv
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
                  [ long "web-port"
                  , metavar "PORT"
                  , value Nothing
                  , help "the port to serve the web interface on"
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
         API.parseServeFlags)
    modifier = fullDesc <> progDesc "Serve."

parseFlags :: Parser Flags
parseFlags = pure Flags
