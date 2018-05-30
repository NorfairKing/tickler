module Tickler.Web.Server.OptParse.Types where

import Import

import Database.Persist.Sqlite
import Servant.Client.Core

import Tickler.API

type Arguments = (Command, Flags)

type Instructions = (Dispatch, Settings)

newtype Command =
    CommandServe ServeFlags
    deriving (Show, Eq)

data ServeFlags = ServeFlags
    { serveFlagPort :: Maybe Int
    , serveFlagPersistLogins :: Maybe Bool
    , serveFlagDefaultIntrayUrl :: Maybe BaseUrl
    , serveFlagAPIPort :: Maybe Int
    , serveFlagAPIDB :: Maybe Text
    , serveFlagAPIConnectionCount :: Maybe Int
    , serveFlagAPIAdmins :: [String]
    } deriving (Show, Eq)

data Flags =
    Flags
    deriving (Show, Eq)

data Configuration =
    Configuration
    deriving (Show, Eq)

data Environment = Environment
    { envPort :: Maybe Int
    , envDefaultIntrayUrl :: Maybe BaseUrl
    , envAPIPort :: Maybe Int
    } deriving (Show, Eq)

newtype Dispatch =
    DispatchServe ServeSettings
    deriving (Show)

data ServeSettings = ServeSettings
    { serveSetPort :: Int
    , serveSetPersistLogins :: Bool
    , serveSetDefaultIntrayUrl :: Maybe BaseUrl
    , serveSetAPIPort :: Int
    , serveSetAPIConnectionInfo :: SqliteConnectionInfo
    , serveSetAPIConnectionCount :: Int
    , serveSetAPIAdmins :: [Username]
    } deriving (Show)

data Settings =
    Settings
    deriving (Show, Eq)
