{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}

module Tickler.Web.Server.OptParse
  ( getSettings,
    Settings (..),
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
  setAPIBaseUrl <- case flagAPIBaseUrl <|> envAPIBaseUrl <|> mc confAPIBaseUrl of
    Nothing -> die "No API URL Configured. Try --help to see how to configure it."
    Just burl -> pure burl
  let setPersistLogins = fromMaybe False $ flagPersistLogins <|> envPersistLogins <|> mc confPersistLogins
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
  let envConfigFile = ms "CONFIG_FILE"
  envPort <- mr "PORT"
  envAPIBaseUrl <- mre "API_URL" (left show . parseBaseUrl)
  envPersistLogins <- mr "PERSIST_LOGINS"
  envDefaultIntrayUrl <- mre "DEFAULT_INTRAY_URL" (left show . parseBaseUrl)
  let envTracking = ms "TRACKING"
  let envVerification = ms "SEARCH_CONSOLE_VERIFICATION"
  pure Environment {..}

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
          T.unpack (TE.decodeUtf8 (renderColouredSchemaViaCodec @Configuration))
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
    <*> flag
      Nothing
      (Just True)
      ( mconcat
          [ long "persist-logins",
            help
              "Whether to persist logins accross restarts. This should not be used in production."
          ]
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
