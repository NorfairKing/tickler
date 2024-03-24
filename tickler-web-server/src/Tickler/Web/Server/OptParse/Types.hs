{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE OverloadedStrings #-}

module Tickler.Web.Server.OptParse.Types where

import Autodocodec
import Control.Monad.Logger
import Import
import Servant.Client.Core
import Tickler.Server.OptParse.Types ()

data Flags = Flags
  { flagConfigFile :: !(Maybe FilePath),
    flagAPIBaseUrl :: !(Maybe BaseUrl),
    flagPort :: !(Maybe Int),
    flagLogLevel :: !(Maybe LogLevel),
    flagDefaultIntrayUrl :: !(Maybe BaseUrl),
    flagTracking :: !(Maybe Text),
    flagVerification :: !(Maybe Text)
  }
  deriving (Show, Eq)

data Configuration = Configuration
  { confPort :: !(Maybe Int),
    confLogLevel :: !(Maybe LogLevel),
    confAPIBaseUrl :: !(Maybe BaseUrl),
    confDefaultIntrayUrl :: !(Maybe BaseUrl),
    confTracking :: !(Maybe Text),
    confVerification :: !(Maybe Text)
  }
  deriving stock (Show, Eq, Generic)

instance HasCodec Configuration where
  codec =
    object "Configuration"
      $ Configuration
      <$> optionalField "port" "The port to serve web requests on"
      .= confPort
      <*> optionalField "log-level" "The minimal sevirity of log messages"
      .= confLogLevel
      <*> optionalField "api-url" "The url to contact the api server at"
      .= confAPIBaseUrl
      <*> optionalField "default-intray-url" "The default intray url to fill in for setting up intray triggers"
      .= confDefaultIntrayUrl
      <*> optionalField "tracking" "The google analytics tracking code"
      .= confTracking
      <*> optionalField "verification" "The google search console verification code"
      .= confVerification

data Environment = Environment
  { envConfigFile :: !(Maybe FilePath),
    envAPIBaseUrl :: !(Maybe BaseUrl),
    envPort :: !(Maybe Int),
    envLogLevel :: !(Maybe LogLevel),
    envDefaultIntrayUrl :: !(Maybe BaseUrl),
    envTracking :: !(Maybe Text),
    envVerification :: !(Maybe Text)
  }
  deriving (Show, Eq)

data Settings = Settings
  { setPort :: !Int,
    setLogLevel :: !LogLevel,
    setAPIBaseUrl :: !BaseUrl,
    setDefaultIntrayUrl :: !(Maybe BaseUrl),
    setTracking :: !(Maybe Text),
    setVerification :: !(Maybe Text)
  }
  deriving (Show)
