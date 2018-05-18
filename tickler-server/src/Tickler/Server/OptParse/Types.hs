module Tickler.Server.OptParse.Types where

import Import

import Database.Persist.Sqlite

import Tickler.API

type Arguments = (Command, Flags)

type Instructions = (Dispatch, Settings)

newtype Command =
    CommandServe ServeFlags
    deriving (Show, Eq)

data ServeFlags = ServeFlags
    { serveFlagPort :: Maybe Int
    , serveFlagDb :: Maybe Text
    , serveFlagConnectionCount :: Maybe Int
    , serveFlagAdmins :: [String]
    } deriving (Show, Eq)

data Flags =
    Flags
    deriving (Show, Eq)

data Configuration =
    Configuration
    deriving (Show, Eq)

newtype Dispatch =
    DispatchServe ServeSettings
    deriving (Show)

data Settings =
    Settings
    deriving (Show, Eq)

data ServeSettings = ServeSettings
    { serveSetPort :: Int
    , serveSetConnectionInfo :: SqliteConnectionInfo
    , serveSetConnectionCount :: Int
    , serveSetAdmins :: [Username]
    } deriving (Show)
