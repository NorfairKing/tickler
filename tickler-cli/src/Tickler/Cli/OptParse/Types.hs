{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}

module Tickler.Cli.OptParse.Types where

import Import

import Data.Text (Text)
import Data.Yaml as Yaml
import Data.Time

import Servant.Client

import Tickler.Data

data Arguments =
    Arguments Command
              Flags

data Instructions =
    Instructions Dispatch
                 Settings

data Command
    = CommandRegister RegisterArgs
    | CommandLogin LoginArgs
    | CommandAdd AddArgs
    | CommandLogout
    | CommandSync
    deriving (Show, Eq, Generic)

data RegisterArgs = RegisterArgs
    { registerArgUsername :: Maybe String
    , registerArgPassword :: Maybe String
    } deriving (Show, Eq, Generic)

data LoginArgs = LoginArgs
    { loginArgUsername :: Maybe String
    , loginArgPassword :: Maybe String
    } deriving (Show, Eq, Generic)

data AddArgs = AddArgs
    { addArgContent :: String
    , addArgTickleDate :: String
    , addArgTickleTime :: Maybe String
    } deriving (Show, Eq, Generic)

data Flags = Flags
    { flagConfigFile :: Maybe FilePath
    , flagUrl :: Maybe String
    , flagTicklerDir :: Maybe FilePath
    , flagSyncStrategy :: Maybe SyncStrategy
    } deriving (Show, Eq, Generic)

data Configuration = Configuration
    { configUrl :: Maybe String
    , configUsername :: Maybe Username
    , configTicklerDir :: Maybe FilePath
    , configSyncStrategy :: Maybe SyncStrategy
    } deriving (Show, Eq, Generic)

instance FromJSON Configuration where
    parseJSON =
        withObject "Configuration" $ \o ->
            Configuration <$> o .:? "url" <*> o .:? "username" <*>
            o .:? "tickler-dir" <*>
            o .:? "sync"

emptyConfiguration :: Configuration
emptyConfiguration =
    Configuration
    { configUrl = Nothing
    , configUsername = Nothing
    , configTicklerDir = Nothing
    , configSyncStrategy = Nothing
    }

data Settings = Settings
    { setBaseUrl :: Maybe BaseUrl
    , setUsername :: Maybe Username
    , setTicklerDir :: Path Abs Dir
    , setSyncStrategy :: SyncStrategy
    } deriving (Show, Eq, Generic)

data SyncStrategy
    = NeverSync
    | AlwaysSync
    deriving (Show, Eq, Generic)

instance FromJSON SyncStrategy

instance ToJSON SyncStrategy

data Dispatch
    = DispatchRegister RegisterSettings
    | DispatchLogin LoginSettings
    | DispatchAdd AddSettings
    | DispatchLogout
    | DispatchSync
    deriving (Show, Eq, Generic)

data RegisterSettings = RegisterSettings
    { registerSetUsername :: Maybe Username
    , registerSetPassword :: Maybe Text
    } deriving (Show, Eq, Generic)

data LoginSettings = LoginSettings
    { loginSetUsername :: Maybe Username
    , loginSetPassword :: Maybe Text
    } deriving (Show, Eq, Generic)

data AddSettings = AddSettings
    { addSetTickleContent :: Text
    , addSetTickleDateTime :: UTCTime
    } deriving (Show, Eq, Generic)

type CliM = ReaderT Settings IO
