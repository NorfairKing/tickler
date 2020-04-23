{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Tickler.Web.Server.OptParse
  ( getInstructions,
    Instructions,
    Dispatch (..),
    Settings (..),
    ServeSettings (..),
  )
where

import qualified Data.Text as T
import Import
import Options.Applicative
import Servant.Client.Core
import qualified System.Environment as System (getArgs, getEnvironment)
import Text.Read
import qualified Tickler.Server.OptParse as API
import Tickler.Web.Server.OptParse.Types

getInstructions :: IO Instructions
getInstructions = do
  (cmd, flags) <- getArguments
  config <- getConfiguration cmd flags
  env <- getEnvironment
  combineToInstructions cmd flags env config

combineToInstructions :: Command -> Flags -> Environment -> Maybe Configuration -> IO Instructions
combineToInstructions (CommandServe ServeFlags {..}) Flags Environment {..} mConf = do
  let mc :: (Configuration -> Maybe a) -> Maybe a
      mc f = mConf >>= f
  API.Instructions (API.DispatchServe apiServeSets) API.Settings <-
    API.combineToInstructions
      (API.CommandServe serveFlagAPIServeFlags)
      API.Flags
      envAPIEnvironment
      (confAPIConf <$> mConf)
  let webHost = serveFlagHost <|> envHost <|> mc confHost
  let webPort = fromMaybe 8000 $ serveFlagPort <|> envPort <|> mc confPort
  when (API.serveSetPort apiServeSets == webPort)
    $ die
    $ unlines
      ["Web server port and API port must not be the same.", "They are both: " ++ show webPort]
  pure
    ( DispatchServe
        ServeSettings
          { serveSetHost = webHost,
            serveSetPort = webPort,
            serveSetPersistLogins = fromMaybe False $ serveFlagPersistLogins <|> envPersistLogins <|> mc confPersistLogins,
            serveSetDefaultIntrayUrl = serveFlagDefaultIntrayUrl <|> envDefaultIntrayUrl <|> mc confDefaultIntrayUrl,
            serveSetTracking = serveFlagTracking <|> envTracking <|> mc confTracking,
            serveSetVerification = serveFlagVerification <|> envVerification <|> mc confVerification,
            serveSetAPISettings = apiServeSets
          },
      Settings
    )

getConfiguration :: Command -> Flags -> IO (Maybe Configuration)
getConfiguration _ _ = pure Nothing

getEnvironment :: IO Environment
getEnvironment = do
  env <- System.getEnvironment
  let ms k = fromString <$> lookup ("TICKLER_WEB_SERVER_" <> k) env
      mre k func =
        forM (ms k) $ \s ->
          case func s of
            Left e ->
              die $
                unwords ["Unable to read ENV Var:", k, "which has value:", show s, "with error:", e]
            Right v -> pure v
      mrf k func =
        mre k $ \s ->
          case func s of
            Nothing -> Left "Parsing failed without a good error message."
            Just v -> Right v
      mr k = mrf k readMaybe
  let envHost = ms "HOST"
  envPort <- mr "PORT"
  envPersistLogins <- mr "PERSIST_LOGINS"
  envDefaultIntrayUrl <- mre "DEFAULT_INTRAY_URL" (left show . parseBaseUrl)
  let envTracking = ms "TRACKING"
  let envVerification = ms "SEARCH_CONSOLE_VERIFICATION"
  envAPIEnvironment <- API.getEnvironment
  pure Environment {..}

getArguments :: IO Arguments
getArguments = do
  args <- System.getArgs
  let result = runArgumentsParser args
  handleParseResult result

runArgumentsParser :: [String] -> ParserResult Arguments
runArgumentsParser = execParserPure prefs_ argParser
  where
    prefs_ =
      ParserPrefs
        { prefMultiSuffix = "",
          prefDisambiguate = True,
          prefShowHelpOnError = True,
          prefShowHelpOnEmpty = True,
          prefBacktrack = True,
          prefColumns = 80
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
      CommandServe
        <$> ( ServeFlags
                <$> option
                  (Just . T.pack <$> str)
                  ( mconcat
                      [ long "web-host",
                        metavar "PORT",
                        value Nothing,
                        help "the host to serve the web interface on"
                      ]
                  )
                <*> option
                  (Just <$> auto)
                  ( mconcat
                      [ long "web-port",
                        metavar "PORT",
                        value Nothing,
                        help "the port to serve the web interface on"
                      ]
                  )
                <*> flag
                  Nothing
                  (Just True)
                  ( mconcat
                      [ long "persist-logins",
                        help
                          "Whether to persist logins accross restarts. This should not be used in production."
                      ]
                  )
                <*> option
                  (Just <$> eitherReader (left show . parseBaseUrl))
                  ( mconcat
                      [ long "default-intray-url",
                        value Nothing,
                        help "The default intray url to suggest when adding an intray trigger."
                      ]
                  )
                <*> option
                  (Just . T.pack <$> str)
                  ( mconcat
                      [ long "analytics-tracking-id",
                        value Nothing,
                        metavar "TRACKING_ID",
                        help "The google analytics tracking ID"
                      ]
                  )
                <*> option
                  (Just . T.pack <$> str)
                  ( mconcat
                      [ long "search-console-verification",
                        value Nothing,
                        metavar "VERIFICATION_TAG",
                        help "The contents of the google search console verification tag"
                      ]
                  )
                <*> API.parseServeFlags
            )
    modifier = fullDesc <> progDesc "Serve."

parseFlags :: Parser Flags
parseFlags = pure Flags
