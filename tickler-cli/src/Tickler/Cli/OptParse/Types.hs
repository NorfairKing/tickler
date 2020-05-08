{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}

module Tickler.Cli.OptParse.Types where

import Import

import Data.Text (Text)
import Data.Time
import Data.Word
import Data.Yaml as Yaml
import YamlParse.Applicative

import Servant.Client

import Tickler.Data

data Arguments =
  Arguments Command Flags

data Instructions =
  Instructions Dispatch Settings

data Command
  = CommandRegister RegisterArgs
  | CommandLogin LoginArgs
  | CommandAdd AddArgs
  | CommandLogout
  | CommandSync
  deriving (Show, Eq, Generic)

data RegisterArgs =
  RegisterArgs
    { registerArgUsername :: Maybe String
    , registerArgPassword :: Maybe String
    }
  deriving (Show, Eq, Generic)

data LoginArgs =
  LoginArgs
    { loginArgUsername :: Maybe String
    , loginArgPassword :: Maybe String
    }
  deriving (Show, Eq, Generic)

data AddArgs =
  AddArgs
    { addArgContent :: String
    , addArgTickleDate :: String
    , addArgTickleTime :: Maybe String
    , addArgRecurrence :: Maybe RecurrenceArgs
    }
  deriving (Show, Eq, Generic)

data RecurrenceArgs
  = RecurrenceArgEveryDayAt (Maybe String)
  | RecurrenceArgEveryDaysAt Word (Maybe String)
  | RecurrenceArgEveryMonthOnAt (Maybe Word8) (Maybe String)
  | RecurrenceArgEveryMonthsOnAt Word (Maybe Word8) (Maybe String)
  deriving (Show, Eq, Generic)

data Flags =
  Flags
    { flagConfigFile :: Maybe FilePath
    , flagUrl :: Maybe String
    , flagCacheDir :: Maybe FilePath
    , flagDataDir :: Maybe FilePath
    , flagSyncStrategy :: Maybe SyncStrategy
    }
  deriving (Show, Eq, Generic)

data Environment =
  Environment
    { envConfigFile :: Maybe FilePath
    , envUrl :: Maybe String
    , envUsername :: Maybe String
    , envPassword :: Maybe String
    , envCacheDir :: Maybe FilePath
    , envDataDir :: Maybe FilePath
    , envSyncStrategy :: Maybe SyncStrategy
    }
  deriving (Show, Eq, Generic)

data Configuration =
  Configuration
    { configUrl :: Maybe String
    , configUsername :: Maybe String
    , configPassword :: Maybe String
    , configCacheDir :: Maybe FilePath
    , configDataDir :: Maybe FilePath
    , configSyncStrategy :: Maybe SyncStrategy
    }
  deriving (Show, Eq, Generic)

instance FromJSON Configuration where
  parseJSON = viaYamlSchema

instance YamlSchema Configuration where
  yamlSchema =
    objectParser "Configuration" $
    Configuration <$>
    optionalField "url" "The api url of the tickler server. Example: api.tickler.cs-syd.eu" <*>
    optionalField "username" "The username to log in with" <*>
    optionalField
      "password"
      "The password to log in with. Note that leaving your password in plaintext in a config file is not safe. Only use this for automation." <*>
    optionalField
      "cache-dir"
      "The directory to store cache information. You can remove this directory as necessary." <*>
    optionalField
      "data-dir"
      "The directory to store data information. Removing this directory could lead to data loss." <*>
    optionalField "sync" "The sync strategy for non-sync commands."

data Settings =
  Settings
    { setBaseUrl :: Maybe BaseUrl
    , setUsername :: Maybe Username
    , setCacheDir :: Path Abs Dir
    , setDataDir :: Path Abs Dir
    , setSyncStrategy :: SyncStrategy
    }
  deriving (Show, Eq, Generic)

data SyncStrategy
  = NeverSync
  | AlwaysSync
  deriving (Show, Read, Eq, Generic)

instance FromJSON SyncStrategy where
  parseJSON = viaYamlSchema

instance ToJSON SyncStrategy

instance YamlSchema SyncStrategy where
  yamlSchema =
    alternatives
      [ literalValue NeverSync <??>
        [ "Only sync when manually running 'intray sync'."
        , "When using this option, you essentially promise that you will take care of ensuring that syncing happens regularly."
        ]
      , literalValue AlwaysSync <??>
        [ "Sync on every change to the local state."
        , "Commands will still succeed even if the sync fails because of internet connect problems for example."
        ]
      ]

data Dispatch
  = DispatchRegister RegisterSettings
  | DispatchLogin LoginSettings
  | DispatchAdd AddSettings
  | DispatchLogout
  | DispatchSync
  deriving (Show, Eq, Generic)

data RegisterSettings =
  RegisterSettings
    { registerSetUsername :: Maybe Username
    , registerSetPassword :: Maybe Text
    }
  deriving (Show, Eq, Generic)

data LoginSettings =
  LoginSettings
    { loginSetUsername :: Maybe Username
    , loginSetPassword :: Maybe Text
    }
  deriving (Show, Eq, Generic)

data AddSettings =
  AddSettings
    { addSetTickleContent :: Text
    , addSetTickleDate :: Day
    , addSetTickleTime :: Maybe TimeOfDay
    , addSetTickleRecurrence :: Maybe Recurrence
    }
  deriving (Show, Eq, Generic)

type CliM = ReaderT Settings IO
