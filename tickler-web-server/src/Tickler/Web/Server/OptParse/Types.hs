{-# LANGUAGE OverloadedStrings #-}

module Tickler.Web.Server.OptParse.Types where

import Data.Aeson
import Import
import Servant.Client.Core
import qualified Tickler.Server.OptParse.Types as API

type Arguments = (Command, Flags)

type Instructions = (Dispatch, Settings)

newtype Command
  = CommandServe ServeFlags
  deriving (Show, Eq)

data ServeFlags
  = ServeFlags
      { serveFlagHost :: Maybe Text,
        serveFlagPort :: Maybe Int,
        serveFlagPersistLogins :: Maybe Bool,
        serveFlagDefaultIntrayUrl :: Maybe BaseUrl,
        serveFlagTracking :: Maybe Text,
        serveFlagVerification :: Maybe Text,
        serveFlagAPIServeFlags :: API.ServeFlags
      }
  deriving (Show, Eq)

data Flags
  = Flags
  deriving (Show, Eq)

data Configuration
  = Configuration
      { confHost :: Maybe Text,
        confPort :: Maybe Int,
        confPersistLogins :: Maybe Bool,
        confDefaultIntrayUrl :: Maybe BaseUrl,
        confTracking :: Maybe Text,
        confVerification :: Maybe Text,
        confAPIConf :: API.Configuration
      }
  deriving (Show, Eq)

instance FromJSON Configuration where
  parseJSON v = flip (withObject "Configuration") v $ \o ->
    Configuration
      <$> o .: "web-host"
      <*> o .: "web-port"
      <*> o .: "persist-logins"
      <*> o .: "default-intray-url"
      <*> o .: "tracking"
      <*> o .: "verification"
      <*> parseJSON v

data Environment
  = Environment
      { envHost :: Maybe Text,
        envPort :: Maybe Int,
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

data ServeSettings
  = ServeSettings
      { serveSetHost :: Maybe Text,
        serveSetPort :: Int,
        serveSetPersistLogins :: Bool,
        serveSetDefaultIntrayUrl :: Maybe BaseUrl,
        serveSetTracking :: Maybe Text,
        serveSetVerification :: Maybe Text,
        serveSetAPISettings :: API.ServeSettings
      }
  deriving (Show)

data Settings
  = Settings
  deriving (Show, Eq)
