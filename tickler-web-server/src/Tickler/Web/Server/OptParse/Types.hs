{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE OverloadedStrings #-}

module Tickler.Web.Server.OptParse.Types where

import Autodocodec
import Import
import Servant.Client.Core
import qualified Tickler.Server.OptParse.Types as API

data Flags = Flags
  { flagPort :: Maybe Int,
    flagPersistLogins :: Maybe Bool,
    flagDefaultIntrayUrl :: Maybe BaseUrl,
    flagTracking :: Maybe Text,
    flagVerification :: Maybe Text,
    flagAPIFlags :: API.Flags
  }
  deriving (Show, Eq)

data Configuration = Configuration
  { confAPIConf :: API.Configuration,
    confPort :: Maybe Int,
    confPersistLogins :: Maybe Bool,
    confDefaultIntrayUrl :: Maybe BaseUrl,
    confTracking :: Maybe Text,
    confVerification :: Maybe Text
  }
  deriving stock (Show, Eq, Generic)

instance HasCodec Configuration where
  codec =
    object "Configuration" $
      Configuration
        <$> API.configurationObjectCodec .= confAPIConf
          <*> optionalField "web-port" "The port to serve web requests on" .= confPort
          <*> optionalField
            "persist-logins"
            "Whether to persist logins accross server restarts. Don't use this in production."
            .= confPersistLogins
          <*> optionalField
            "default-intray-url"
            "The default intray url to fill in for setting up intray triggers"
            .= confDefaultIntrayUrl
          <*> optionalField "tracking" "The google analytics tracking code" .= confTracking
          <*> optionalField "verification" "The google search console verification code" .= confVerification

data Environment = Environment
  { envPort :: Maybe Int,
    envPersistLogins :: Maybe Bool,
    envDefaultIntrayUrl :: Maybe BaseUrl,
    envTracking :: Maybe Text,
    envVerification :: Maybe Text,
    envAPIEnvironment :: API.Environment
  }
  deriving (Show, Eq)

data Settings = Settings
  { setPort :: Int,
    setPersistLogins :: Bool,
    setDefaultIntrayUrl :: Maybe BaseUrl,
    setTracking :: Maybe Text,
    setVerification :: Maybe Text,
    setAPISettings :: API.Settings
  }
  deriving (Show)
