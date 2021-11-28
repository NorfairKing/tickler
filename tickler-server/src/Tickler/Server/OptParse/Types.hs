{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Tickler.Server.OptParse.Types where

import Autodocodec
import Control.Monad.Logger
import qualified Control.Monad.Trans.AWS as AWS
import Data.Aeson (FromJSON, ToJSON)
import Database.Persist.Sqlite
import Import
import Tickler.API
import Web.Stripe.Client as Stripe
import Web.Stripe.Types as Stripe hiding (object)

instance HasCodec () where
  codec = nullCodec

data Arguments
  = Arguments Command Flags

data Instructions
  = Instructions Dispatch Settings

newtype Command
  = CommandServe ServeFlags
  deriving (Show, Eq)

data ServeFlags = ServeFlags
  { serveFlagPort :: Maybe Int,
    serveFlagWebHost :: Maybe Text,
    serveFlagLogLevel :: Maybe LogLevel,
    serveFlagDb :: Maybe Text,
    serveFlagAdmins :: [Username],
    serveFlagFreeloaders :: [Username],
    serveFlagsMonetisationFlags :: MonetisationFlags,
    serveFlagsLooperFlags :: LoopersFlags
  }
  deriving (Show, Eq)

data MonetisationFlags = MonetisationFlags
  { monetisationFlagStripePlan :: !(Maybe String),
    monetisationFlagStripeSecretKey :: !(Maybe String),
    monetisationFlagStripePublishableKey :: !(Maybe String),
    monetisationFlagLooperStripeEventsFetcher :: LooperFlagsWith (),
    monetisationFlagLooperStripeEventsRetrier :: LooperFlagsWith (),
    monetisationFlagMaxItemsFree :: !(Maybe Int)
  }
  deriving (Show, Eq)

data LoopersFlags = LoopersFlags
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
    looperFlagTriggeredEmailConverterFlags :: LooperFlagsWith (Maybe EmailAddress),
    looperFlagAdminNotificationEmailConverterFlags :: LooperFlagsWith AdminNotificationEmailConverterFlags
  }
  deriving (Show, Eq)

data AdminNotificationEmailConverterFlags = AdminNotificationEmailConverterFlags
  { adminNotificationEmailConverterFlagFromAddress :: Maybe EmailAddress,
    adminNotificationEmailConverterFlagToAddress :: Maybe EmailAddress
  }
  deriving (Show, Eq)

data LooperFlagsWith a = LooperFlagsWith
  { looperFlagEnable :: Maybe Bool,
    looperFlagsPeriod :: Maybe Int,
    looperFlagsRetryPolicy :: LooperFlagsRetryPolicy,
    looperFlags :: a
  }
  deriving (Show, Eq)

data LooperFlagsRetryPolicy = LooperFlagsRetryPolicy
  { looperFlagsRetryDelay :: Maybe Int,
    looperFlagsRetryAmount :: Maybe Int
  }
  deriving (Show, Eq)

data Flags = Flags
  { flagConfigFile :: Maybe FilePath
  }
  deriving (Show, Eq)

data Configuration = Configuration
  { confDb :: !(Maybe Text),
    confWebHost :: !(Maybe Text),
    confPort :: !(Maybe Int),
    confLogLevel :: !(Maybe LogLevel),
    confAdmins :: !(Maybe [Username]),
    confFreeloaders :: !(Maybe [Username]),
    confMonetisationConfiguration :: !(Maybe MonetisationConfiguration),
    confLoopersConfiguration :: !(Maybe LoopersConfiguration)
  }
  deriving stock (Show, Eq, Generic)
  deriving (FromJSON, ToJSON) via (Autodocodec Configuration)

instance HasCodec Configuration where
  codec = object "Configuration" configurationObjectCodec

configurationObjectCodec :: JSONObjectCodec Configuration
configurationObjectCodec =
  Configuration <$> optionalField "database" "The database file" .= confDb
    <*> optionalField
      "web-host"
      "The host to serve the web-server on, this is used to to send emails with links to the web interface"
      .= confWebHost
    <*> optionalField "api-port" "The port to serve the api-server on" .= confPort
    <*> optionalField "log-level" "The minimal sevirity of log messages" .= confLogLevel
    <*> optionalField "admins" "The list of usernames that will be considered administrators" .= confAdmins
    <*> optionalField "freeloaders" "The list of usernames that won't have to pay" .= confFreeloaders
    <*> optionalField
      "monetisation"
      "Monetisation configuration. If this is not configured then the server is run for free."
      .= confMonetisationConfiguration
    <*> optionalField "loopers" "The configuration for all the loopers" .= confLoopersConfiguration

data MonetisationConfiguration = MonetisationConfiguration
  { monetisationConfStripePlan :: !(Maybe String),
    monetisationConfStripeSecretKey :: !(Maybe String),
    monetisationConfStripePulishableKey :: !(Maybe String),
    monetisationConfLooperStripeEventsFetcher :: !(Maybe (LooperConfWith ())),
    monetisationConfLooperStripeEventsRetrier :: !(Maybe (LooperConfWith ())),
    monetisationConfMaxItemsFree :: !(Maybe Int)
  }
  deriving stock (Show, Eq, Generic)
  deriving (FromJSON, ToJSON) via (Autodocodec MonetisationConfiguration)

instance HasCodec MonetisationConfiguration where
  codec =
    object "MonetisationConfiguration" $
      MonetisationConfiguration
        <$> optionalField
          "stripe-plan"
          "The stripe identifier of the stripe plan used to checkout a subscription"
          .= monetisationConfStripePlan
        <*> optionalField "stripe-secret-key" "The secret key for calling the stripe api" .= monetisationConfStripeSecretKey
        <*> optionalField "stripe-publishable-key" "The publishable key for calling the stripe api" .= monetisationConfStripePulishableKey
        <*> optionalField "stripe-events-fetcher" "The configuration for the stripe events fetcher" .= monetisationConfLooperStripeEventsFetcher
        <*> optionalField "stripe-events-retrier" "The configuration for the stripe events fetcher" .= monetisationConfLooperStripeEventsRetrier
        <*> optionalField "max-items-free" "The number of items a free user can have on the server" .= monetisationConfMaxItemsFree

data LoopersConfiguration = LoopersConfiguration
  { looperConfDefaultEnabled :: !(Maybe Bool),
    looperConfDefaultPeriod :: !(Maybe Int),
    looperConfDefaultRetryDelay :: !(Maybe Int),
    looperConfDefaultRetryAmount :: !(Maybe Int),
    looperConfTriggererConf :: !(Maybe (LooperConfWith ())),
    looperConfEmailerConf :: !(Maybe (LooperConfWith ())),
    looperConfTriggeredIntrayItemSchedulerConf :: !(Maybe (LooperConfWith ())),
    looperConfTriggeredIntrayItemSenderConf :: !(Maybe (LooperConfWith ())),
    looperConfVerificationEmailConverterConf :: !(Maybe (LooperConfWith VerificationEmailConverterConf)),
    looperConfTriggeredEmailSchedulerConf :: !(Maybe (LooperConfWith ())),
    looperConfTriggeredEmailConverterConf :: !(Maybe (LooperConfWith TriggeredEmailConverterConf)),
    looperConfAdminNotificationEmailConverterConf :: !(Maybe (LooperConfWith AdminNotificationEmailConverterConf))
  }
  deriving stock (Show, Eq, Generic)
  deriving (FromJSON, ToJSON) via (Autodocodec LoopersConfiguration)

instance HasCodec LoopersConfiguration where
  codec =
    object "LoopersConfiguration" $
      LoopersConfiguration
        <$> optionalField "default-enabled" "Whether to enable any given looper by default" .= looperConfDefaultEnabled
        <*> optionalField "default-period" "The default period for any given looper by default (in seconds)" .= looperConfDefaultPeriod
        <*> optionalField
          "default-retry-delay"
          "The default delay to retry for any given looper by default (in microseconds)"
          .= looperConfDefaultRetryDelay
        <*> optionalField
          "default-retry-times"
          "The default number of times to retry for any given looper by default (in microseconds)"
          .= looperConfDefaultRetryAmount
        <*> optionalField "triggerer" "The looper that triggers tickles" .= looperConfTriggererConf
        <*> optionalField "emailer" "The looper that sends emails" .= looperConfEmailerConf
        <*> optionalField
          "triggered-intray-item-scheduler"
          "The looper that schedules adding intray items for a triggered tickle"
          .= looperConfTriggeredIntrayItemSchedulerConf
        <*> optionalField
          "triggered-intray-item-sender"
          "The looper that actually adds intray items for a triggered tickle"
          .= looperConfTriggeredIntrayItemSenderConf
        <*> optionalField
          "verification-email-converter"
          "The looper that converts verification emails in the database to actual emails"
          .= looperConfVerificationEmailConverterConf
        <*> optionalField
          "triggered-email-scheduler"
          "The looper that schedules sending emails for a triggered tickle"
          .= looperConfTriggeredEmailSchedulerConf
        <*> optionalField
          "triggered-email-converter"
          "The looper that converts triggered item emails in the database to actual emails"
          .= looperConfTriggeredEmailConverterConf
        <*> optionalField
          "admin-notification-email-converter"
          "The looper that converts admin notifications to actual emails"
          .= looperConfAdminNotificationEmailConverterConf

data LooperConfWith a = LooperConfWith
  { looperConfEnable :: !(Maybe Bool),
    looperConfPeriod :: !(Maybe Int),
    looperConfRetryPolicy :: !(Maybe LooperConfRetryPolicy),
    looperConf :: Maybe a
  }
  deriving stock (Show, Eq, Generic)
  deriving (FromJSON, ToJSON) via (Autodocodec (LooperConfWith a))

instance HasCodec a => HasCodec (LooperConfWith a) where
  codec =
    object "LooperConfWith" $
      LooperConfWith
        <$> optionalField "enable" "Wether to enable the looper" .= looperConfEnable
        <*> optionalField "period" "The period between runs of the looper" .= looperConfPeriod
        <*> optionalField "retry-policy" "The retry policy of the looper" .= looperConfRetryPolicy
        <*> optionalField "conf" "The looper-specific configuration of the looper" .= looperConf

data LooperConfRetryPolicy = LooperConfRetryPolicy
  { looperConfRetryDelay :: !(Maybe Int),
    looperConfRetryAmount :: !(Maybe Int)
  }
  deriving stock (Show, Eq, Generic)
  deriving (FromJSON, ToJSON) via (Autodocodec LooperConfRetryPolicy)

instance HasCodec LooperConfRetryPolicy where
  codec =
    object "LooperConfRetryPolicy" $
      LooperConfRetryPolicy
        <$> optionalField "delay" "The delay to retry the looper (in microseconds)" .= looperConfRetryDelay
        <*> optionalField "amount" "The number of times to retry the looper" .= looperConfRetryAmount

data VerificationEmailConverterConf = VerificationEmailConverterConf
  { verificationEmailConverterConfFromAddress :: Maybe EmailAddress
  }
  deriving stock (Show, Eq, Generic)
  deriving (FromJSON, ToJSON) via (Autodocodec VerificationEmailConverterConf)

instance HasCodec VerificationEmailConverterConf where
  codec =
    object "VerificationEmailConverterConf" $
      VerificationEmailConverterConf
        <$> optionalField "from" "The from address for verification emails" .= verificationEmailConverterConfFromAddress

data TriggeredEmailConverterConf = TriggeredEmailConverterConf
  { triggeredEmailConverterConfFromAddress :: Maybe EmailAddress
  }
  deriving stock (Show, Eq, Generic)
  deriving (FromJSON, ToJSON) via (Autodocodec TriggeredEmailConverterConf)

instance HasCodec TriggeredEmailConverterConf where
  codec =
    object "TriggeredEmailConverterConf" $
      TriggeredEmailConverterConf
        <$> optionalField "from" "The from address for triggered item emails" .= triggeredEmailConverterConfFromAddress

data AdminNotificationEmailConverterConf = AdminNotificationEmailConverterConf
  { adminNotificationEmailConverterConfFromAddress :: Maybe EmailAddress,
    adminNotificationEmailConverterConfToAddress :: Maybe EmailAddress
  }
  deriving stock (Show, Eq, Generic)
  deriving (FromJSON, ToJSON) via (Autodocodec AdminNotificationEmailConverterConf)

instance HasCodec AdminNotificationEmailConverterConf where
  codec =
    object "AdminNotificationEmailConverterConf" $
      AdminNotificationEmailConverterConf
        <$> optionalField "from" "The 'from' address for admin notification emails" .= adminNotificationEmailConverterConfFromAddress
        <*> optionalField "to" "The 'to' address for admin notification emails" .= adminNotificationEmailConverterConfToAddress

data Environment = Environment
  { envConfigFile :: Maybe FilePath,
    envDb :: Maybe Text,
    envWebHost :: Maybe Text,
    envPort :: Maybe Int,
    envLogLevel :: Maybe LogLevel,
    envMonetisationEnvironment :: MonetisationEnvironment,
    envLoopersEnvironment :: LoopersEnvironment
  }
  deriving (Show, Eq)

data MonetisationEnvironment = MonetisationEnvironment
  { monetisationEnvStripePlan :: !(Maybe String),
    monetisationEnvStripeSecretKey :: !(Maybe String),
    monetisationEnvStripePulishableKey :: !(Maybe String),
    monetisationEnvLooperStripeEventsFetcher :: LooperEnvWith (),
    monetisationEnvLooperStripeEventsRetrier :: LooperEnvWith (),
    monetisationEnvMaxItemsFree :: !(Maybe Int)
  }
  deriving (Show, Eq)

data LoopersEnvironment = LoopersEnvironment
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
    looperEnvTriggeredEmailConverterEnv :: LooperEnvWith (Maybe EmailAddress),
    looperEnvAdminNotificationEmailConverterEnv :: LooperEnvWith AdminNotificationEmailConverterEnvironment
  }
  deriving (Show, Eq)

data LooperEnvWith a = LooperEnvWith
  { looperEnvEnable :: Maybe Bool,
    looperEnvPeriod :: Maybe Int,
    looperEnvRetryPolicy :: LooperEnvRetryPolicy,
    looperEnv :: a
  }
  deriving (Show, Eq)

data LooperEnvRetryPolicy = LooperEnvRetryPolicy
  { looperEnvRetryDelay :: Maybe Int,
    looperEnvRetryAmount :: Maybe Int
  }
  deriving (Show, Eq)

data AdminNotificationEmailConverterEnvironment = AdminNotificationEmailConverterEnvironment
  { adminNotificationEmailConverterEnvFromAddress :: Maybe EmailAddress,
    adminNotificationEmailConverterEnvToAddress :: Maybe EmailAddress
  }
  deriving (Show, Eq)

newtype Dispatch
  = DispatchServe ServeSettings
  deriving (Show)

data Settings
  = Settings
  deriving (Show, Eq)

data ServeSettings = ServeSettings
  { serveSetPort :: !Int,
    serveSetLogLevel :: !LogLevel,
    serveSetConnectionInfo :: !SqliteConnectionInfo,
    serveSetAdmins :: ![Username],
    serveSetFreeloaders :: ![Username],
    serveSetMonetisationSettings :: !(Maybe MonetisationSettings),
    serveSetLoopersSettings :: !LoopersSettings
  }
  deriving (Show)

data MonetisationSettings = MonetisationSettings
  { monetisationSetStripeSettings :: !StripeSettings,
    monetisationSetStripeEventsFetcher :: !(LooperSetsWith ()),
    monetisationSetStripeEventsRetrier :: !(LooperSetsWith ()),
    monetisationSetMaxItemsFree :: !Int
  }
  deriving (Show)

data StripeSettings = StripeSettings
  { stripeSetPlan :: !Stripe.PlanId,
    stripeSetStripeConfig :: StripeConfig,
    stripeSetPublishableKey :: !Text
  }
  deriving (Show)

data LoopersSettings = LoopersSettings
  { looperSetTriggererSets :: LooperSetsWith (),
    looperSetEmailerSets :: LooperSetsWith EmailerSettings,
    looperSetTriggeredIntrayItemSchedulerSets :: LooperSetsWith (),
    looperSetTriggeredIntrayItemSenderSets :: LooperSetsWith (),
    looperSetVerificationEmailConverterSets :: LooperSetsWith VerificationEmailConverterSettings,
    looperSetTriggeredEmailSchedulerSets :: LooperSetsWith (),
    looperSetTriggeredEmailConverterSets :: LooperSetsWith TriggeredEmailConverterSettings,
    looperSetAdminNotificationEmailConverterSets :: LooperSetsWith AdminNotificationEmailConverterSettings
  }
  deriving (Show)

data LooperSetsWith a
  = LooperEnabled LooperStaticConfig a -- Int number of seconds
  | LooperDisabled
  deriving (Show, Eq)

data LooperStaticConfig = LooperStaticConfig
  { looperStaticConfigPeriod :: Int,
    looperStaticConfigRetryPolicy :: LooperRetryPolicy
  }
  deriving (Show, Eq)

data LooperRetryPolicy = LooperRetryPolicy
  { looperRetryPolicyDelay :: Int, -- Microseconds
    looperRetryPolicyAmount :: Int
  }
  deriving (Show, Eq)

data EmailerSettings = EmailerSettings
  { emailerSetAWSCredentials :: AWS.Credentials
  }
  deriving (Show)

data VerificationEmailConverterSettings = VerificationEmailConverterSettings
  { verificationEmailConverterSetFromAddress :: !EmailAddress,
    verificationEmailConverterSetFromName :: !Text,
    verificationEmailConverterSetWebHost :: !Text
  }
  deriving (Show)

data TriggeredEmailConverterSettings = TriggeredEmailConverterSettings
  { triggeredEmailConverterSetFromAddress :: !EmailAddress,
    triggeredEmailConverterSetFromName :: !Text,
    triggeredEmailConverterSetWebHost :: !Text
  }
  deriving (Show)

data AdminNotificationEmailConverterSettings = AdminNotificationEmailConverterSettings
  { adminNotificationEmailConverterSetFromAddress :: !EmailAddress,
    adminNotificationEmailConverterSetFromName :: !Text,
    adminNotificationEmailConverterSetToAddress :: !EmailAddress,
    adminNotificationEmailConverterSetToName :: !Text,
    adminNotificationEmailConverterSetWebHost :: !Text
  }
  deriving (Show)
