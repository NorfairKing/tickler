{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE OverloadedStrings #-}

module Tickler.Web.Server.OptParse.Types where

import Autodocodec
import Import
import Servant.Client.Core
import qualified Tickler.Server.OptParse.Types as API

type Arguments = (Command, Flags)

type Instructions = (Dispatch, Settings)

newtype Command
  = CommandServe ServeFlags
  deriving (Show, Eq)

data ServeFlags = ServeFlags
  { serveFlagPort :: Maybe Int,
    serveFlagPersistLogins :: Maybe Bool,
    serveFlagDefaultIntrayUrl :: Maybe BaseUrl,
    serveFlagTracking :: Maybe Text,
    serveFlagVerification :: Maybe Text,
    serveFlagAPIServeFlags :: API.Flags
  }
  deriving (Show, Eq)

data Flags = Flags
  { flagAPIFlags :: API.Flags
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

newtype Dispatch
  = DispatchServe ServeSettings
  deriving (Show)

data ServeSettings = ServeSettings
  { serveSetPort :: Int,
    serveSetPersistLogins :: Bool,
    serveSetDefaultIntrayUrl :: Maybe BaseUrl,
    serveSetTracking :: Maybe Text,
    serveSetVerification :: Maybe Text,
    serveSetAPISettings :: API.Settings
  }
  deriving (Show)

data Settings
  = Settings
  deriving (Show, Eq)
