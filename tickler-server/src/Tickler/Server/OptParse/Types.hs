module Tickler.Server.OptParse.Types where

import Import

import Control.Monad.Trans.AWS as AWS
import Database.Persist.Sqlite

import Looper

import Tickler.API

data Arguments =
  Arguments Command Flags

data Instructions =
  Instructions Dispatch Settings

newtype Command =
  CommandServe ServeFlags
  deriving (Show, Eq)

data ServeFlags =
  ServeFlags
    { serveFlagPort :: Maybe Int
    , serveFlagWebHost :: Maybe String
    , serveFlagDb :: Maybe FilePath
    , serveFlagConnectionCount :: Maybe Int
    , serveFlagAdmins :: [String]
    , serveFlagsLooperFlags :: LoopersFlags
    }
  deriving (Show, Eq)

data LoopersFlags =
  LoopersFlags
    { looperFlagDefaultEnabled :: Maybe Bool
    , looperFlagDefaultPeriod :: Maybe Int
    , looperFlagDefaultRetryDelay :: Maybe Int
    , looperFlagDefaultRetryTimes :: Maybe Int
    , looperFlagTriggererFlags :: LooperFlags
    , looperFlagEmailerFlags :: LooperFlags
    , looperFlagTriggeredIntrayItemSchedulerFlags :: LooperFlags
    , looperFlagTriggeredIntrayItemSenderFlags :: LooperFlags
    , looperFlagVerificationEmailConverterFlags :: LooperFlags
    , looperFlagTriggeredEmailSchedulerFlags :: LooperFlags
    , looperFlagTriggeredEmailConverterFlags :: LooperFlags
    }
  deriving (Show, Eq)

data Flags =
  Flags
  deriving (Show, Eq)

data Configuration =
  Configuration
  deriving (Show, Eq)

data Environment =
  Environment
    { envDb :: Maybe FilePath
    , envWebHost :: Maybe String
    , envPort :: Maybe Int
    , envLoopersEnvironment :: LoopersEnvironment
    }
  deriving (Show, Eq)

data LoopersEnvironment =
  LoopersEnvironment
    { looperEnvDefaultEnabled :: Maybe Bool
    , looperEnvDefaultPeriod :: Maybe Int
    , looperEnvDefaultRetryDelay :: Maybe Int
    , looperEnvDefaultRetryTimes :: Maybe Int
    , looperEnvTriggererEnv :: LooperEnvironment
    , looperEnvEmailerEnv :: LooperEnvironment
    , looperEnvTriggeredIntrayItemSchedulerEnv :: LooperEnvironment
    , looperEnvTriggeredIntrayItemSenderEnv :: LooperEnvironment
    , looperEnvVerificationEmailConverterEnv :: LooperEnvironment
    , looperEnvTriggeredEmailSchedulerEnv :: LooperEnvironment
    , looperEnvTriggeredEmailConverterEnv :: LooperEnvironment
    }
  deriving (Show, Eq)

newtype Dispatch =
  DispatchServe ServeSettings
  deriving (Show)

data Settings =
  Settings
  deriving (Show, Eq)

data ServeSettings =
  ServeSettings
    { serveSetPort :: Int
    , serveSetConnectionInfo :: SqliteConnectionInfo
    , serveSetConnectionCount :: Int
    , serveSetAdmins :: [Username]
    , serveSetLooperSettings :: LoopersSettings
    }
  deriving (Show)

data LoopersSettings =
  LoopersSettings
    { looperSetConnectionInfo :: SqliteConnectionInfo
    , looperSetConnectionCount :: Int
    , looperSetEmailerSets :: LooperSetsWith EmailerSettings
    , looperSetTriggererSets :: LooperSetsWith TriggererSettings
    , looperSetTriggeredIntrayItemSchedulerSets :: LooperSetsWith ()
    , looperSetTriggeredIntrayItemSenderSets :: LooperSetsWith ()
    , looperSetVerificationEmailConverterSets :: LooperSetsWith VerificationEmailConverterSettings
    , looperSetTriggeredEmailSchedulerSets :: LooperSetsWith ()
    , looperSetTriggeredEmailConverterSets :: LooperSetsWith TriggeredEmailConverterSettings
    }
  deriving (Show)

data LooperSetsWith a
  = LooperEnabled LooperSettings a -- Int number of seconds
  | LooperDisabled
  deriving (Show)

data TriggererSettings =
  TriggererSettings
  deriving (Show)

data EmailerSettings =
  EmailerSettings
    { emailerSetAWSCredentials :: AWS.Credentials
    }
  deriving (Show)

data VerificationEmailConverterSettings =
  VerificationEmailConverterSettings
    { verificationEmailConverterSetFromAddress :: !EmailAddress
    , verificationEmailConverterSetFromName :: !Text
    , verificationEmailConverterSetWebHost :: !Text
    }
  deriving (Show)

data TriggeredEmailConverterSettings =
  TriggeredEmailConverterSettings
    { triggeredEmailConverterSetFromAddress :: !EmailAddress
    , triggeredEmailConverterSetFromName :: !Text
    , triggeredEmailConverterSetWebHost :: !Text
    }
  deriving (Show)
