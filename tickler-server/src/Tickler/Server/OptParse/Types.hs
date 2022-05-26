{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Tickler.Server.OptParse.Types where

import Autodocodec
import Control.Monad.Logger
import Data.Aeson (FromJSON, ToJSON)
import Import
import Looper
import Tickler.API

instance HasCodec () where
  codec = nullCodec

data Flags = Flags
  { flagConfigFile :: Maybe FilePath,
    flagPort :: Maybe Int,
    flagWebHost :: Maybe Text,
    flagLogLevel :: Maybe LogLevel,
    flagDb :: Maybe String,
    flagAdmins :: [Username],
    flagFreeloaders :: [Username],
    flagsMonetisationFlags :: MonetisationFlags,
    flagTriggererFromEmailAddress :: !(Maybe EmailAddress),
    flagVerificationFromEmailAddress :: !(Maybe EmailAddress),
    flagAdminNotificationFromEmailAddress :: !(Maybe EmailAddress),
    flagAdminNotificationToEmailAddress :: !(Maybe EmailAddress),
    flagTriggererFlags :: LooperFlags,
    flagEmailerFlags :: LooperFlags,
    flagTriggeredIntrayItemSchedulerFlags :: LooperFlags,
    flagTriggeredIntrayItemSenderFlags :: LooperFlags,
    flagVerificationEmailConverterFlags :: LooperFlags,
    flagTriggeredEmailSchedulerFlags :: LooperFlags,
    flagTriggeredEmailConverterFlags :: LooperFlags,
    flagAdminNotificationEmailConverterFlags :: LooperFlags
  }
  deriving (Show, Eq)

data MonetisationFlags = MonetisationFlags
  { monetisationFlagStripePlan :: !(Maybe Text),
    monetisationFlagStripeSecretKey :: !(Maybe Text),
    monetisationFlagStripePublishableKey :: !(Maybe Text),
    monetisationFlagMaxItemsFree :: !(Maybe Int),
    monetisationFlagPrice :: !(Maybe Text)
  }
  deriving (Show, Eq)

data Configuration = Configuration
  { confDb :: !(Maybe String),
    confWebHost :: !(Maybe Text),
    confPort :: !(Maybe Int),
    confLogLevel :: !(Maybe LogLevel),
    confAdmins :: !(Maybe [Username]),
    confFreeloaders :: !(Maybe [Username]),
    confMonetisationConfiguration :: !(Maybe MonetisationConfiguration),
    confTriggererFromEmailAddress :: !(Maybe EmailAddress),
    confVerificationFromEmailAddress :: !(Maybe EmailAddress),
    confAdminNotificationFromEmailAddress :: !(Maybe EmailAddress),
    confAdminNotificationToEmailAddress :: !(Maybe EmailAddress),
    confTriggererConf :: !(Maybe LooperConfiguration),
    confEmailerConf :: !(Maybe LooperConfiguration),
    confTriggeredIntrayItemSchedulerConf :: !(Maybe LooperConfiguration),
    confTriggeredIntrayItemSenderConf :: !(Maybe LooperConfiguration),
    confVerificationEmailConverterConf :: !(Maybe LooperConfiguration),
    confTriggeredEmailSchedulerConf :: !(Maybe LooperConfiguration),
    confTriggeredEmailConverterConf :: !(Maybe LooperConfiguration),
    confAdminNotificationEmailConverterConf :: !(Maybe LooperConfiguration)
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
    <*> optionalField
      "port"
      "The port to serve the api-server on"
      .= confPort
    <*> optionalField
      "log-level"
      "The minimal sevirity of log messages"
      .= confLogLevel
    <*> optionalField
      "admins"
      "The list of usernames that will be considered administrators"
      .= confAdmins
    <*> optionalField
      "freeloaders"
      "The list of usernames that won't have to pay"
      .= confFreeloaders
    <*> optionalField
      "monetisation"
      "Monetisation configuration. If this is not configured then the server is run for free."
      .= confMonetisationConfiguration
    <*> optionalField
      "triggerer-from"
      "From email address for triggered emails"
      .= confTriggererFromEmailAddress
    <*> optionalField
      "verification-from"
      "From email address for verification emails"
      .= confVerificationFromEmailAddress
    <*> optionalField
      "admin-notification-from"
      "From email address for admin notifcitaion emails"
      .= confAdminNotificationFromEmailAddress
    <*> optionalField
      "admin-notification-to"
      "To email address for admin notifcitaion emails"
      .= confAdminNotificationToEmailAddress
    <*> optionalField "triggerer" "The looper that triggers tickles" .= confTriggererConf
    <*> optionalField "emailer" "The looper that sends emails" .= confEmailerConf
    <*> optionalField
      "triggered-intray-item-scheduler"
      "The looper that schedules adding intray items for a triggered tickle"
      .= confTriggeredIntrayItemSchedulerConf
    <*> optionalField
      "triggered-intray-item-sender"
      "The looper that actually adds intray items for a triggered tickle"
      .= confTriggeredIntrayItemSenderConf
    <*> optionalField
      "verification-email-converter"
      "The looper that converts verification emails in the database to actual emails"
      .= confVerificationEmailConverterConf
    <*> optionalField
      "triggered-email-scheduler"
      "The looper that schedules sending emails for a triggered tickle"
      .= confTriggeredEmailSchedulerConf
    <*> optionalField
      "triggered-email-converter"
      "The looper that converts triggered item emails in the database to actual emails"
      .= confTriggeredEmailConverterConf
    <*> optionalField
      "admin-notification-email-converter"
      "The looper that converts admin notifications to actual emails"
      .= confAdminNotificationEmailConverterConf

data MonetisationConfiguration = MonetisationConfiguration
  { monetisationConfStripePlan :: !(Maybe Text),
    monetisationConfStripeSecretKey :: !(Maybe Text),
    monetisationConfStripePulishableKey :: !(Maybe Text),
    monetisationConfMaxItemsFree :: !(Maybe Int),
    monetisationConfPrice :: !(Maybe Text)
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
        <*> optionalField
          "stripe-secret-key"
          "The secret key for calling the stripe api"
          .= monetisationConfStripeSecretKey
        <*> optionalField
          "stripe-publishable-key"
          "The publishable key for calling the stripe api"
          .= monetisationConfStripePulishableKey
        <*> optionalFieldOrNull
          "max-items-free"
          "The number of items a free user can have on the server"
          .= monetisationConfMaxItemsFree
        <*> optionalFieldOrNull "price" "A string description of the price" .= monetisationConfPrice

data Environment = Environment
  { envConfigFile :: Maybe FilePath,
    envDb :: Maybe String,
    envWebHost :: Maybe Text,
    envPort :: Maybe Int,
    envLogLevel :: Maybe LogLevel,
    envMonetisationEnvironment :: MonetisationEnvironment,
    envTriggererFromEmailAddress :: !(Maybe EmailAddress),
    envVerificationFromEmailAddress :: !(Maybe EmailAddress),
    envAdminNotificationFromEmailAddress :: !(Maybe EmailAddress),
    envAdminNotificationToEmailAddress :: !(Maybe EmailAddress),
    envTriggererEnv :: LooperEnvironment,
    envEmailerEnv :: LooperEnvironment,
    envTriggeredIntrayItemSchedulerEnv :: LooperEnvironment,
    envTriggeredIntrayItemSenderEnv :: LooperEnvironment,
    envVerificationEmailConverterEnv :: LooperEnvironment,
    envTriggeredEmailSchedulerEnv :: LooperEnvironment,
    envTriggeredEmailConverterEnv :: LooperEnvironment,
    envAdminNotificationEmailConverterEnv :: LooperEnvironment
  }
  deriving (Show, Eq)

data MonetisationEnvironment = MonetisationEnvironment
  { monetisationEnvStripePlan :: !(Maybe Text),
    monetisationEnvStripeSecretKey :: !(Maybe Text),
    monetisationEnvStripePulishableKey :: !(Maybe Text),
    monetisationEnvMaxItemsFree :: !(Maybe Int),
    monetisationEnvPrice :: !(Maybe Text)
  }
  deriving (Show, Eq)

data Settings = Settings
  { setPort :: !Int,
    setWebHost :: !(Maybe Text),
    setLogLevel :: !LogLevel,
    setDb :: !(Path Abs File),
    setAdmins :: ![Username],
    setFreeloaders :: ![Username],
    setMonetisationSettings :: !(Maybe MonetisationSettings),
    setTriggererFromEmailAddress :: !(Maybe EmailAddress),
    setVerificationFromEmailAddress :: !(Maybe EmailAddress),
    setAdminNotificationFromEmailAddress :: !(Maybe EmailAddress),
    setAdminNotificationToEmailAddress :: !(Maybe EmailAddress),
    setTriggererSets :: !LooperSettings,
    setEmailerSets :: !LooperSettings,
    setTriggeredIntrayItemSchedulerSets :: !LooperSettings,
    setTriggeredIntrayItemSenderSets :: !LooperSettings,
    setVerificationEmailConverterSets :: !LooperSettings,
    setTriggeredEmailSchedulerSets :: !LooperSettings,
    setTriggeredEmailConverterSets :: !LooperSettings,
    setAdminNotificationEmailConverterSets :: !LooperSettings
  }
  deriving (Show)

data MonetisationSettings = MonetisationSettings
  { monetisationSetStripeSettings :: !StripeSettings,
    monetisationSetMaxItemsFree :: !Int,
    monetisationSetPrice :: !Text
  }
  deriving (Show)

data StripeSettings = StripeSettings
  { stripeSetPlan :: !Text,
    stripeSetSecretKey :: Text,
    stripeSetPublishableKey :: !Text
  }
  deriving (Show)
