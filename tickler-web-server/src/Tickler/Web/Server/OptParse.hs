{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}

module Tickler.Web.Server.OptParse
  ( getSettings,
    Settings (..),
  )
where

import Autodocodec.Yaml
import Control.Monad.Logger
import qualified Data.Text as T
import qualified Env
import Import
import Options.Applicative as OptParse
import qualified Options.Applicative.Help as OptParse
import Servant.Client.Core
import qualified System.Environment as System (getArgs)
import Tickler.Web.Server.OptParse.Types

getSettings :: IO Settings
getSettings = do
  flags <- getFlags
  env <- getEnvironment
  config <- getConfiguration flags env
  combineToSettings flags env config

combineToSettings :: Flags -> Environment -> Maybe Configuration -> IO Settings
combineToSettings Flags {..} Environment {..} mConf = do
  let mc :: (Configuration -> Maybe a) -> Maybe a
      mc f = mConf >>= f
  let setPort = fromMaybe 8080 $ flagPort <|> envPort <|> mc confPort
  let setLogLevel = fromMaybe LevelInfo $ flagLogLevel <|> envLogLevel <|> mc confLogLevel
  setAPIBaseUrl <- case flagAPIBaseUrl <|> envAPIBaseUrl <|> mc confAPIBaseUrl of
    Nothing -> die "No API URL Configured. Try --help to see how to configure it."
    Just burl -> pure burl
  let setDefaultIntrayUrl = flagDefaultIntrayUrl <|> envDefaultIntrayUrl <|> mc confDefaultIntrayUrl
  let setTracking = flagTracking <|> envTracking <|> mc confTracking
  let setVerification = flagVerification <|> envVerification <|> mc confVerification
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
  Env.prefixed "TICKLER_WEB_SERVER_"
    $ Environment
    <$> optional (Env.var Env.str "CONFIG_FILE" (Env.help "configuration file"))
    <*> optional (Env.var (left (Env.UnreadError . show) . parseBaseUrl) "API_URL" (Env.help "base url of the api server"))
    <*> optional (Env.var Env.auto "PORT" (Env.help "port to run the web server on"))
    <*> optional (Env.var Env.auto "LOG_LEVEL" (Env.help "minimal severity of log messages"))
    <*> optional (Env.var (left (Env.UnreadError . show) . parseBaseUrl) "DEFAULT_INTRAY_URL" (Env.help "Default intray url to suggest when adding intray triggers"))
    <*> optional (Env.var Env.str "TRACKING" (Env.help "Tracking code"))
    <*> optional (Env.var Env.str "SEARCH_CONSOLE_VERIFICATION" (Env.help "Search console verification"))

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
          T.unpack (renderColouredSchemaViaCodec @Configuration)
        ]

parseFlags :: Parser Flags
parseFlags =
  Flags
    <$> optional
      ( strOption
          ( mconcat
              [ long "config-file",
                metavar "FILEPATH",
                help "configuration file"
              ]
          )
      )
    <*> optional
      ( option
          (eitherReader (left show . parseBaseUrl))
          ( mconcat
              [ long "api-url",
                help "The url to contact the api server at"
              ]
          )
      )
    <*> optional
      ( option
          auto
          ( mconcat
              [ long "port",
                metavar "PORT",
                help "the port to serve the web interface on"
              ]
          )
      )
    <*> optional
      ( option
          auto
          ( mconcat
              [ long "log-level",
                metavar "LOG_LEVEL",
                help "minimal severity of log messages"
              ]
          )
      )
    <*> optional
      ( option
          (eitherReader (left show . parseBaseUrl))
          ( mconcat
              [ long "default-intray-url",
                help "The default intray url to suggest when adding an intray trigger."
              ]
          )
      )
    <*> optional
      ( strOption
          ( mconcat
              [ long "analytics-tracking-id",
                metavar "TRACKING_ID",
                help "The google analytics tracking ID"
              ]
          )
      )
    <*> optional
      ( strOption
          ( mconcat
              [ long "search-console-verification",
                metavar "VERIFICATION_TAG",
                help "The contents of the google search console verification tag"
              ]
          )
      )
