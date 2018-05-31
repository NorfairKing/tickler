module Tickler.Web.Server.OptParse.Types where

import Import

import Servant.Client.Core

import qualified Tickler.Server.OptParse.Types as API

type Arguments = (Command, Flags)

type Instructions = (Dispatch, Settings)

newtype Command =
    CommandServe ServeFlags
    deriving (Show, Eq)

data ServeFlags = ServeFlags
    { serveFlagPort :: Maybe Int
    , serveFlagPersistLogins :: Maybe Bool
    , serveFlagDefaultIntrayUrl :: Maybe BaseUrl
    , serveFlagAPIServeFlags :: API.ServeFlags
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
    , envAPIEnvironment :: API.Environment
    } deriving (Show, Eq)

newtype Dispatch =
    DispatchServe ServeSettings
    deriving (Show)

data ServeSettings = ServeSettings
    { serveSetPort :: Int
    , serveSetPersistLogins :: Bool
    , serveSetDefaultIntrayUrl :: Maybe BaseUrl
    , serveSetAPISettings :: API.ServeSettings
    } deriving (Show)

data Settings =
    Settings
    deriving (Show, Eq)
