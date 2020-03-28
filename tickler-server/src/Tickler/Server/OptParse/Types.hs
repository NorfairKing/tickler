module Tickler.Server.OptParse.Types where

import Import

import Control.Monad.Trans.AWS as AWS
import Database.Persist.Sqlite

import Web.Stripe.Client as Stripe
import Web.Stripe.Types as Stripe

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
    , serveFlagAdmins :: [String]
    , serveFlagsMonetisationFlags :: MonetisationFlags
    , serveFlagsLooperFlags :: LooperFlags
    }
  deriving (Show, Eq)

data MonetisationFlags =
  MonetisationFlags
    { monetisationFlagStripePlan :: !(Maybe String)
    , monetisationFlagStripeSecretKey :: !(Maybe String)
    , monetisationFlagStripePublishableKey :: !(Maybe String)
    , monetisationFlagLooperStripeEventsFetcher :: LooperFlagsWith ()
    , monetisationFlagLooperStripeEventsRetrier :: LooperFlagsWith ()
    , monetisationFlagMaxItemsFree :: !(Maybe Int)
    }
  deriving (Show, Eq)

data LooperFlags =
  LooperFlags
    { looperFlagDefaultEnabled :: Maybe Bool
    , looperFlagDefaultPeriod :: Maybe Int
    , looperFlagDefaultRetryDelay :: Maybe Int
    , looperFlagDefaultRetryTimes :: Maybe Int
    , looperFlagTriggererFlags :: LooperFlagsWith ()
    , looperFlagEmailerFlags :: LooperFlagsWith ()
    , looperFlagTriggeredIntrayItemSchedulerFlags :: LooperFlagsWith ()
    , looperFlagTriggeredIntrayItemSenderFlags :: LooperFlagsWith ()
    , looperFlagVerificationEmailConverterFlags :: LooperFlagsWith (Maybe EmailAddress)
    , looperFlagTriggeredEmailSchedulerFlags :: LooperFlagsWith ()
    , looperFlagTriggeredEmailConverterFlags :: LooperFlagsWith (Maybe EmailAddress)
    }
  deriving (Show, Eq)

data LooperFlagsWith a =
  LooperFlagsWith
    { looperFlagEnable :: Maybe Bool
    , looperFlagsPeriod :: Maybe Int
    , looperFlagsRetryPolicy :: LooperFlagsRetryPolicy
    , looperFlags :: a
    }
  deriving (Show, Eq)

data LooperFlagsRetryPolicy =
  LooperFlagsRetryPolicy
    { looperFlagsRetryDelay :: Maybe Int
    , looperFlagsRetryAmount :: Maybe Int
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
    , envMonetisationEnvironment :: MonetisationEnvironment
    , envLoopersEnvironment :: LoopersEnvironment
    }
  deriving (Show, Eq)

data MonetisationEnvironment =
  MonetisationEnvironment
    { monetisationEnvStripePlan :: !(Maybe String)
    , monetisationEnvStripeSecretKey :: !(Maybe String)
    , monetisationEnvStripePulishableKey :: !(Maybe String)
    , monetisationEnvLooperStripeEventsFetcher :: LooperEnvWith ()
    , monetisationEnvLooperStripeEventsRetrier :: LooperEnvWith ()
    , monetisationEnvMaxItemsFree :: !(Maybe Int)
    }
  deriving (Show, Eq)

data LoopersEnvironment =
  LoopersEnvironment
    { looperEnvDefaultEnabled :: Maybe Bool
    , looperEnvDefaultPeriod :: Maybe Int
    , looperEnvDefaultRetryDelay :: Maybe Int
    , looperEnvDefaultRetryTimes :: Maybe Int
    , looperEnvTriggererEnv :: LooperEnvWith ()
    , looperEnvEmailerEnv :: LooperEnvWith ()
    , looperEnvTriggeredIntrayItemSchedulerEnv :: LooperEnvWith ()
    , looperEnvTriggeredIntrayItemSenderEnv :: LooperEnvWith ()
    , looperEnvVerificationEmailConverterEnv :: LooperEnvWith (Maybe EmailAddress)
    , looperEnvTriggeredEmailSchedulerEnv :: LooperEnvWith ()
    , looperEnvTriggeredEmailConverterEnv :: LooperEnvWith (Maybe EmailAddress)
    }
  deriving (Show, Eq)

data LooperEnvWith a =
  LooperEnvWith
    { looperEnvEnable :: Maybe Bool
    , looperEnvPeriod :: Maybe Int
    , looperEnvRetryPolicy :: LooperEnvRetryPolicy
    , looperEnv :: a
    }
  deriving (Show, Eq)

data LooperEnvRetryPolicy =
  LooperEnvRetryPolicy
    { looperEnvRetryDelay :: Maybe Int
    , looperEnvRetryAmount :: Maybe Int
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
    { serveSetPort :: !Int
    , serveSetConnectionInfo :: !SqliteConnectionInfo
    , serveSetAdmins :: ![Username]
    , serveSetMonetisationSettings :: !(Maybe MonetisationSettings)
    , serveSetLooperSettings :: !LooperSettings
    }
  deriving (Show)

data MonetisationSettings =
  MonetisationSettings
    { monetisationSetStripeSettings :: !StripeSettings
    , monetisationSetStripeEventsFetcher :: !(LooperSetsWith ())
    , monetisationSetStripeEventsRetrier :: !(LooperSetsWith ())
    , monetisationSetMaxItemsFree :: !Int
    }
  deriving (Show)

data StripeSettings =
  StripeSettings
    { stripeSetPlan :: !Stripe.PlanId
    , stripeSetStripeConfig :: !StripeConfig
    , stripeSetPublishableKey :: !Text
    }
  deriving (Show)

data LooperSettings =
  LooperSettings
    { looperSetTriggererSets :: LooperSetsWith TriggererSettings
    , looperSetEmailerSets :: LooperSetsWith EmailerSettings
    , looperSetTriggeredIntrayItemSchedulerSets :: LooperSetsWith ()
    , looperSetTriggeredIntrayItemSenderSets :: LooperSetsWith ()
    , looperSetVerificationEmailConverterSets :: LooperSetsWith VerificationEmailConverterSettings
    , looperSetTriggeredEmailSchedulerSets :: LooperSetsWith ()
    , looperSetTriggeredEmailConverterSets :: LooperSetsWith TriggeredEmailConverterSettings
    }
  deriving (Show)

data LooperSetsWith a
  = LooperEnabled LooperStaticConfig a -- Int number of seconds
  | LooperDisabled
  deriving (Show, Eq)

data LooperStaticConfig =
  LooperStaticConfig
    { looperStaticConfigPeriod :: Int
    , looperStaticConfigRetryPolicy :: LooperRetryPolicy
    }
  deriving (Show, Eq)

data LooperRetryPolicy =
  LooperRetryPolicy
    { looperRetryPolicyDelay :: Int -- Microseconds
    , looperRetryPolicyAmount :: Int
    }
  deriving (Show, Eq)

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
