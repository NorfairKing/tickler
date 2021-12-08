{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}

module Tickler.Web.Server.OptParse
  ( getInstructions,
    Instructions,
    Dispatch (..),
    Settings (..),
    ServeSettings (..),
  )
where

import Autodocodec.Yaml
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Import
import Options.Applicative as OptParse
import qualified Options.Applicative.Help as OptParse
import Servant.Client.Core
import qualified System.Environment as System (getArgs, getEnvironment)
import Text.Read
import qualified Tickler.Server.OptParse as API
import Tickler.Web.Server.OptParse.Types

getInstructions :: IO Instructions
getInstructions = do
  (cmd, flags) <- getArguments
  env <- getEnvironment
  config <- getConfiguration flags env
  combineToInstructions cmd flags env config

combineToInstructions :: Command -> Flags -> Environment -> Maybe Configuration -> IO Instructions
combineToInstructions (CommandServe ServeFlags {..}) Flags {..} Environment {..} mConf = do
  let mc :: (Configuration -> Maybe a) -> Maybe a
      mc f = mConf >>= f
  apiSets <-
    API.combineToSettings
      flagAPIFlags
      envAPIEnvironment
      (confAPIConf <$> mConf)
  let webPort = fromMaybe 8000 $ serveFlagPort <|> envPort <|> mc confPort
  when (API.setPort apiSets == webPort) $
    die $
      unlines
        ["Web server port and API port must not be the same.", "They are both: " ++ show webPort]
  pure
    ( DispatchServe
        ServeSettings
          { serveSetPort = webPort,
            serveSetPersistLogins =
              fromMaybe False $ serveFlagPersistLogins <|> envPersistLogins <|> mc confPersistLogins,
            serveSetDefaultIntrayUrl =
              serveFlagDefaultIntrayUrl <|> envDefaultIntrayUrl <|> mc confDefaultIntrayUrl,
            serveSetTracking = serveFlagTracking <|> envTracking <|> mc confTracking,
            serveSetVerification = serveFlagVerification <|> envVerification <|> mc confVerification,
            serveSetAPISettings = apiSets
          },
      Settings
    )

getConfiguration :: Flags -> Environment -> IO (Maybe Configuration)
getConfiguration Flags {..} Environment {..} = do
  configFile <-
    case API.flagConfigFile flagAPIFlags <|> API.envConfigFile envAPIEnvironment of
      Nothing -> API.getDefaultConfigFile
      Just cf -> resolveFile' cf
  readYamlConfigFile configFile

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
    prefs_ = defaultPrefs {prefShowHelpOnError = True, prefShowHelpOnEmpty = True}

argParser :: ParserInfo Arguments
argParser = info (helper <*> parseArgs) help_
  where
    help_ = fullDesc <> footerDoc (Just $ OptParse.string footerStr)
    footerStr =
      unlines
        [ "Configuration file format:",
          T.unpack (TE.decodeUtf8 (renderColouredSchemaViaCodec @Configuration))
        ]

parseArgs :: Parser Arguments
parseArgs = (,) <$> parseCommand <*> parseFlags

parseCommand :: Parser Command
parseCommand = hsubparser $ mconcat [command "serve" parseCommandServe]

parseCommandServe :: ParserInfo Command
parseCommandServe = info parser help_
  where
    parser =
      CommandServe
        <$> ( ServeFlags
                <$> option
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
                <*> API.parseFlags
            )
    help_ = fullDesc <> footerDoc (Just $ OptParse.string footerStr)
    footerStr =
      unlines
        [ "Configuration file format:",
          T.unpack (TE.decodeUtf8 (renderColouredSchemaViaCodec @Configuration))
        ]

parseFlags :: Parser Flags
parseFlags = Flags <$> API.parseFlags
