{-# LANGUAGE OverloadedStrings #-}

module Tickler.Server.OptParse.Types where

import Control.Monad.Trans.AWS as AWS
import Data.Aeson
import Database.Persist.Sqlite
import Import
import Tickler.API
import Web.Stripe.Client as Stripe
import Web.Stripe.Types as Stripe

data Arguments
  = Arguments Command Flags

data Instructions
  = Instructions Dispatch Settings

newtype Command
  = CommandServe ServeFlags
  deriving (Show, Eq)

data ServeFlags
  = ServeFlags
      { serveFlagPort :: Maybe Int,
        serveFlagWebHost :: Maybe String,
        serveFlagDb :: Maybe Text,
        serveFlagAdmins :: [String],
        serveFlagsMonetisationFlags :: MonetisationFlags,
        serveFlagsLooperFlags :: LoopersFlags
      }
  deriving (Show, Eq)

data MonetisationFlags
  = MonetisationFlags
      { monetisationFlagStripePlan :: !(Maybe String),
        monetisationFlagStripeSecretKey :: !(Maybe String),
        monetisationFlagStripePublishableKey :: !(Maybe String),
        monetisationFlagLooperStripeEventsFetcher :: LooperFlagsWith (),
        monetisationFlagLooperStripeEventsRetrier :: LooperFlagsWith (),
        monetisationFlagMaxItemsFree :: !(Maybe Int)
      }
  deriving (Show, Eq)

data LoopersFlags
  = LoopersFlags
      { looperFlagDefaultEnabled :: Maybe Bool,
        looperFlagDefaultPeriod :: Maybe Int,
        looperFlagDefaultRetryDelay :: Maybe Int,
        looperFlagDefaultRetryAmount :: Maybe Int,
        looperFlagTriggererFlags :: LooperFlagsWith (),
        looperFlagEmailerFlags :: LooperFlagsWith (),
        looperFlagTriggeredIntrayItemSchedulerFlags :: LooperFlagsWith (),
        looperFlagTriggeredIntrayItemSenderFlags :: LooperFlagsWith (),
        looperFlagVerificationEmailConverterFlags :: LooperFlagsWith (Maybe EmailAddress),
        looperFlagTriggeredEmailSchedulerFlags :: LooperFlagsWith (),
        looperFlagTriggeredEmailConverterFlags :: LooperFlagsWith (Maybe EmailAddress)
      }
  deriving (Show, Eq)

data LooperFlagsWith a
  = LooperFlagsWith
      { looperFlagEnable :: Maybe Bool,
        looperFlagsPeriod :: Maybe Int,
        looperFlagsRetryPolicy :: LooperFlagsRetryPolicy,
        looperFlags :: a
      }
  deriving (Show, Eq)

data LooperFlagsRetryPolicy
  = LooperFlagsRetryPolicy
      { looperFlagsRetryDelay :: Maybe Int,
        looperFlagsRetryAmount :: Maybe Int
      }
  deriving (Show, Eq)

data Flags
  = Flags
  deriving (Show, Eq)

data Configuration
  = Configuration
      { confDb :: !(Maybe Text),
        confWebHost :: !(Maybe String),
        confPort :: !(Maybe Int),
        confMonetisationConfiguration :: !(Maybe MonetisationConfiguration),
        confLoopersConfiguration :: !(Maybe LoopersConfiguration)
      }
  deriving (Show, Eq)

instance FromJSON Configuration where
  parseJSON = withObject "Configuration" $ \o ->
    Configuration <$> o .: "database" <*> o .: "web-host" <*> o .: "api-port" <*> o .: "monetisation" <*> o .: "loopers"

data MonetisationConfiguration
  = MonetisationConfiguration
      { monetisationConfStripePlan :: !(Maybe String),
        monetisationConfStripeSecretKey :: !(Maybe String),
        monetisationConfStripePulishableKey :: !(Maybe String),
        monetisationConfLooperStripeEventsFetcher :: !(Maybe (LooperConfWith ())),
        monetisationConfLooperStripeEventsRetrier :: !(Maybe (LooperConfWith ())),
        monetisationConfMaxItemsFree :: !(Maybe Int)
      }
  deriving (Show, Eq)

instance FromJSON MonetisationConfiguration where
  parseJSON = withObject "MonetisationConfiguration" $ \o ->
    MonetisationConfiguration
      <$> o .: "stripe-plan"
      <*> o .: "stripe-secret-key"
      <*> o .: "stripe-publishable-key"
      <*> o .: "stripe-stripe-events-fetcher"
      <*> o .: "stripe-stripe-events-retrier"
      <*> o .: "max-items-free"

data LoopersConfiguration
  = LoopersConfiguration
      { looperConfDefaultEnabled :: !(Maybe Bool),
        looperConfDefaultPeriod :: !(Maybe Int),
        looperConfDefaultRetryDelay :: !(Maybe Int),
        looperConfDefaultRetryAmount :: !(Maybe Int),
        looperConfTriggererConf :: !(Maybe (LooperConfWith ())),
        looperConfEmailerConf :: !(Maybe (LooperConfWith ())),
        looperConfTriggeredIntrayItemSchedulerConf :: !(Maybe (LooperConfWith ())),
        looperConfTriggeredIntrayItemSenderConf :: !(Maybe (LooperConfWith ())),
        looperConfVerificationEmailConverterConf :: !(Maybe (LooperConfWith (Maybe EmailAddress))),
        looperConfTriggeredEmailSchedulerConf :: !(Maybe (LooperConfWith ())),
        looperConfTriggeredEmailConverterConf :: !(Maybe (LooperConfWith (Maybe EmailAddress)))
      }
  deriving (Show, Eq)

instance FromJSON LoopersConfiguration where
  parseJSON = withObject "LoopersConfiguration" $ \o ->
    LoopersConfiguration
      <$> o .:? "default-enabled"
      <*> o .:? "default-period"
      <*> o .:? "default-retry-delay"
      <*> o .:? "default-retry-times"
      <*> o .:? "triggerer"
      <*> o .:? "emailer"
      <*> o .:? "triggered-intray-item-scheduler"
      <*> o .:? "triggered-intray-item-sender"
      <*> o .:? "verification-email-converter"
      <*> o .:? "triggered-email-scheduler"
      <*> o .:? "triggered-email-converter"

data LooperConfWith a
  = LooperConfWith
      { looperConfEnable :: !(Maybe Bool),
        looperConfPeriod :: !(Maybe Int),
        looperConfRetryPolicy :: !(Maybe LooperConfRetryPolicy),
        looperConf :: Maybe a
      }
  deriving (Show, Eq)

instance FromJSON a => FromJSON (LooperConfWith a) where
  parseJSON = withObject "LooperConfWith" $ \o ->
    LooperConfWith
      <$> o .:? "enable"
      <*> o .:? "period"
      <*> o .:? "retry-policy"
      <*> o .:? "conf"

data LooperConfRetryPolicy
  = LooperConfRetryPolicy
      { looperConfRetryDelay :: !(Maybe Int),
        looperConfRetryAmount :: !(Maybe Int)
      }
  deriving (Show, Eq)

instance FromJSON LooperConfRetryPolicy where
  parseJSON = withObject "LooperConfRetryPolicy" $ \o ->
    LooperConfRetryPolicy
      <$> o .:? "delay"
      <*> o .:? "amount"

data Environment
  = Environment
      { envDb :: Maybe Text,
        envWebHost :: Maybe String,
        envPort :: Maybe Int,
        envMonetisationEnvironment :: MonetisationEnvironment,
        envLoopersEnvironment :: LoopersEnvironment
      }
  deriving (Show, Eq)

data MonetisationEnvironment
  = MonetisationEnvironment
      { monetisationEnvStripePlan :: !(Maybe String),
        monetisationEnvStripeSecretKey :: !(Maybe String),
        monetisationEnvStripePulishableKey :: !(Maybe String),
        monetisationEnvLooperStripeEventsFetcher :: LooperEnvWith (),
        monetisationEnvLooperStripeEventsRetrier :: LooperEnvWith (),
        monetisationEnvMaxItemsFree :: !(Maybe Int)
      }
  deriving (Show, Eq)

data LoopersEnvironment
  = LoopersEnvironment
      { looperEnvDefaultEnabled :: Maybe Bool,
        looperEnvDefaultPeriod :: Maybe Int,
        looperEnvDefaultRetryDelay :: Maybe Int,
        looperEnvDefaultRetryAmount :: Maybe Int,
        looperEnvTriggererEnv :: LooperEnvWith (),
        looperEnvEmailerEnv :: LooperEnvWith (),
        looperEnvTriggeredIntrayItemSchedulerEnv :: LooperEnvWith (),
        looperEnvTriggeredIntrayItemSenderEnv :: LooperEnvWith (),
        looperEnvVerificationEmailConverterEnv :: LooperEnvWith (Maybe EmailAddress),
        looperEnvTriggeredEmailSchedulerEnv :: LooperEnvWith (),
        looperEnvTriggeredEmailConverterEnv :: LooperEnvWith (Maybe EmailAddress)
      }
  deriving (Show, Eq)

data LooperEnvWith a
  = LooperEnvWith
      { looperEnvEnable :: Maybe Bool,
        looperEnvPeriod :: Maybe Int,
        looperEnvRetryPolicy :: LooperEnvRetryPolicy,
        looperEnv :: a
      }
  deriving (Show, Eq)

data LooperEnvRetryPolicy
  = LooperEnvRetryPolicy
      { looperEnvRetryDelay :: Maybe Int,
        looperEnvRetryAmount :: Maybe Int
      }
  deriving (Show, Eq)

newtype Dispatch
  = DispatchServe ServeSettings
  deriving (Show)

data Settings
  = Settings
  deriving (Show, Eq)

data ServeSettings
  = ServeSettings
      { serveSetPort :: !Int,
        serveSetConnectionInfo :: !SqliteConnectionInfo,
        serveSetAdmins :: ![Username],
        serveSetMonetisationSettings :: !(Maybe MonetisationSettings),
        serveSetLoopersSettings :: !LoopersSettings
      }
  deriving (Show)

data MonetisationSettings
  = MonetisationSettings
      { monetisationSetStripeSettings :: !StripeSettings,
        monetisationSetStripeEventsFetcher :: !(LooperSetsWith ()),
        monetisationSetStripeEventsRetrier :: !(LooperSetsWith ()),
        monetisationSetMaxItemsFree :: !Int
      }
  deriving (Show)

data StripeSettings
  = StripeSettings
      { stripeSetPlan :: !Stripe.PlanId,
        stripeSetStripeConfig :: StripeConfig,
        stripeSetPublishableKey :: !Text
      }
  deriving (Show)

data LoopersSettings
  = LoopersSettings
      { looperSetTriggererSets :: LooperSetsWith (),
        looperSetEmailerSets :: LooperSetsWith EmailerSettings,
        looperSetTriggeredIntrayItemSchedulerSets :: LooperSetsWith (),
        looperSetTriggeredIntrayItemSenderSets :: LooperSetsWith (),
        looperSetVerificationEmailConverterSets :: LooperSetsWith VerificationEmailConverterSettings,
        looperSetTriggeredEmailSchedulerSets :: LooperSetsWith (),
        looperSetTriggeredEmailConverterSets :: LooperSetsWith TriggeredEmailConverterSettings
      }
  deriving (Show)

data LooperSetsWith a
  = LooperEnabled LooperStaticConfig a -- Int number of seconds
  | LooperDisabled
  deriving (Show, Eq)

data LooperStaticConfig
  = LooperStaticConfig
      { looperStaticConfigPeriod :: Int,
        looperStaticConfigRetryPolicy :: LooperRetryPolicy
      }
  deriving (Show, Eq)

data LooperRetryPolicy
  = LooperRetryPolicy
      { looperRetryPolicyDelay :: Int, -- Microseconds
        looperRetryPolicyAmount :: Int
      }
  deriving (Show, Eq)

data EmailerSettings
  = EmailerSettings
      { emailerSetAWSCredentials :: AWS.Credentials
      }
  deriving (Show)

data VerificationEmailConverterSettings
  = VerificationEmailConverterSettings
      { verificationEmailConverterSetFromAddress :: !EmailAddress,
        verificationEmailConverterSetFromName :: !Text,
        verificationEmailConverterSetWebHost :: !Text
      }
  deriving (Show)

data TriggeredEmailConverterSettings
  = TriggeredEmailConverterSettings
      { triggeredEmailConverterSetFromAddress :: !EmailAddress,
        triggeredEmailConverterSetFromName :: !Text,
        triggeredEmailConverterSetWebHost :: !Text
      }
  deriving (Show)
