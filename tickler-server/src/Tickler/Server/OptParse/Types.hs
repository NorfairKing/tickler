{-# LANGUAGE OverloadedStrings #-}

module Tickler.Server.OptParse.Types where

import Control.Monad.Trans.AWS as AWS
import Data.Aeson
import Database.Persist.Sqlite
import Import
import Tickler.API
import Web.Stripe.Client as Stripe
import Web.Stripe.Types as Stripe
import YamlParse.Applicative

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
    , serveFlagDb :: Maybe Text
    , serveFlagAdmins :: [Username]
    , serveFlagFreeloaders :: [Username]
    , serveFlagsMonetisationFlags :: MonetisationFlags
    , serveFlagsLooperFlags :: LoopersFlags
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

data LoopersFlags =
  LoopersFlags
    { looperFlagDefaultEnabled :: Maybe Bool
    , looperFlagDefaultPeriod :: Maybe Int
    , looperFlagDefaultRetryDelay :: Maybe Int
    , looperFlagDefaultRetryAmount :: Maybe Int
    , looperFlagTriggererFlags :: LooperFlagsWith ()
    , looperFlagEmailerFlags :: LooperFlagsWith ()
    , looperFlagTriggeredIntrayItemSchedulerFlags :: LooperFlagsWith ()
    , looperFlagTriggeredIntrayItemSenderFlags :: LooperFlagsWith ()
    , looperFlagVerificationEmailConverterFlags :: LooperFlagsWith (Maybe EmailAddress)
    , looperFlagTriggeredEmailSchedulerFlags :: LooperFlagsWith ()
    , looperFlagTriggeredEmailConverterFlags :: LooperFlagsWith (Maybe EmailAddress)
    , looperFlagAdminNotificationEmailConverterFlags :: LooperFlagsWith AdminNotificationEmailConverterFlags
    }
  deriving (Show, Eq)

data AdminNotificationEmailConverterFlags =
  AdminNotificationEmailConverterFlags
    { adminNotificationEmailConverterFlagFromAddress :: Maybe EmailAddress
    , adminNotificationEmailConverterFlagToAddress :: Maybe EmailAddress
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
    { flagConfigFile :: Maybe FilePath
    }
  deriving (Show, Eq)

data Configuration =
  Configuration
    { confDb :: !(Maybe Text)
    , confWebHost :: !(Maybe String)
    , confPort :: !(Maybe Int)
    , confAdmins :: !(Maybe [Username])
    , confFreeloaders :: !(Maybe [Username])
    , confMonetisationConfiguration :: !(Maybe MonetisationConfiguration)
    , confLoopersConfiguration :: !(Maybe LoopersConfiguration)
    }
  deriving (Show, Eq)

instance FromJSON Configuration where
  parseJSON = viaYamlSchema

instance YamlSchema Configuration where
  yamlSchema =
    objectParser "Configuration" $
    Configuration <$> optionalField "database" "The database file" <*>
    optionalField
      "web-host"
      "The host to serve the web-server on, this is used to to send emails with links to the web interface" <*>
    optionalField "api-port" "The port to serve the api-server on" <*>
    optionalField "admins" "The list of usernames that will be considered administrators" <*>
    optionalField "freeloaders" "The list of usernames that won't have to pay" <*>
    optionalField
      "monetisation"
      "Monetisation configuration. If this is not configured then the server is run for free." <*>
    optionalField "loopers" "The configuration for all the loopers"

data MonetisationConfiguration =
  MonetisationConfiguration
    { monetisationConfStripePlan :: !(Maybe String)
    , monetisationConfStripeSecretKey :: !(Maybe String)
    , monetisationConfStripePulishableKey :: !(Maybe String)
    , monetisationConfLooperStripeEventsFetcher :: !(Maybe (LooperConfWith ()))
    , monetisationConfLooperStripeEventsRetrier :: !(Maybe (LooperConfWith ()))
    , monetisationConfMaxItemsFree :: !(Maybe Int)
    }
  deriving (Show, Eq)

instance FromJSON MonetisationConfiguration where
  parseJSON = viaYamlSchema

instance YamlSchema MonetisationConfiguration where
  yamlSchema =
    objectParser "MonetisationConfiguration" $
    MonetisationConfiguration <$>
    optionalField
      "stripe-plan"
      "The stripe identifier of the stripe plan used to checkout a subscription" <*>
    optionalField "stripe-secret-key" "The secret key for calling the stripe api" <*>
    optionalField "stripe-publishable-key" "The publishable key for calling the stripe api" <*>
    optionalField "stripe-events-fetcher" "The configuration for the stripe events fetcher" <*>
    optionalField "stripe-events-retrier" "The configuration for the stripe events fetcher" <*>
    optionalField "max-items-free" "The number of items a free user can have on the server"

data LoopersConfiguration =
  LoopersConfiguration
    { looperConfDefaultEnabled :: !(Maybe Bool)
    , looperConfDefaultPeriod :: !(Maybe Int)
    , looperConfDefaultRetryDelay :: !(Maybe Int)
    , looperConfDefaultRetryAmount :: !(Maybe Int)
    , looperConfTriggererConf :: !(Maybe (LooperConfWith ()))
    , looperConfEmailerConf :: !(Maybe (LooperConfWith ()))
    , looperConfTriggeredIntrayItemSchedulerConf :: !(Maybe (LooperConfWith ()))
    , looperConfTriggeredIntrayItemSenderConf :: !(Maybe (LooperConfWith ()))
    , looperConfVerificationEmailConverterConf :: !(Maybe (LooperConfWith VerificationEmailConverterConf))
    , looperConfTriggeredEmailSchedulerConf :: !(Maybe (LooperConfWith ()))
    , looperConfTriggeredEmailConverterConf :: !(Maybe (LooperConfWith TriggeredEmailConverterConf))
    , looperConfAdminNotificationEmailConverterConf :: !(Maybe (LooperConfWith AdminNotificationEmailConverterConf))
    }
  deriving (Show, Eq)

instance FromJSON LoopersConfiguration where
  parseJSON = viaYamlSchema

instance YamlSchema LoopersConfiguration where
  yamlSchema =
    objectParser "LoopersConfiguration" $
    LoopersConfiguration <$>
    optionalField "default-enabled" "Whether to enable any given looper by default" <*>
    optionalField "default-period" "The default period for any given looper by default (in seconds)" <*>
    optionalField
      "default-retry-delay"
      "The default delay to retry for any given looper by default (in microseconds)" <*>
    optionalField
      "default-retry-times"
      "The default number of times to retry for any given looper by default (in microseconds)" <*>
    optionalField "triggerer" "The looper that triggers tickles" <*>
    optionalField "emailer" "The looper that sends emails" <*>
    optionalField
      "triggered-intray-item-scheduler"
      "The looper that schedules adding intray items for a triggered tickle" <*>
    optionalField
      "triggered-intray-item-sender"
      "The looper that actually adds intray items for a triggered tickle" <*>
    optionalField
      "verification-email-converter"
      "The looper that converts verification emails in the database to actual emails" <*>
    optionalField
      "triggered-email-scheduler"
      "The looper that schedules sending emails for a triggered tickle" <*>
    optionalField
      "triggered-email-converter"
      "The looper that converts triggered item emails in the database to actual emails" <*>
    optionalField
      "admin-notification-email-converter"
      "The looper that converts admin notifications to actual emails"

data LooperConfWith a =
  LooperConfWith
    { looperConfEnable :: !(Maybe Bool)
    , looperConfPeriod :: !(Maybe Int)
    , looperConfRetryPolicy :: !(Maybe LooperConfRetryPolicy)
    , looperConf :: Maybe a
    }
  deriving (Show, Eq)

instance YamlSchema a => FromJSON (LooperConfWith a) where
  parseJSON = viaYamlSchema

instance YamlSchema a => YamlSchema (LooperConfWith a) where
  yamlSchema =
    objectParser "LooperConfWith" $
    LooperConfWith <$> optionalField "enable" "Wether to enable the looper" <*>
    optionalField "period" "The period between runs of the looper" <*>
    optionalField "retry-policy" "The retry policy of the looper" <*>
    optionalField "conf" "The looper-specific configuration of the looper"

data LooperConfRetryPolicy =
  LooperConfRetryPolicy
    { looperConfRetryDelay :: !(Maybe Int)
    , looperConfRetryAmount :: !(Maybe Int)
    }
  deriving (Show, Eq)

instance FromJSON LooperConfRetryPolicy where
  parseJSON = viaYamlSchema

instance YamlSchema LooperConfRetryPolicy where
  yamlSchema =
    objectParser "LooperConfRetryPolicy" $
    LooperConfRetryPolicy <$>
    optionalField "delay" "The delay to retry the looper (in microseconds)" <*>
    optionalField "amount" "The number of times to retry the looper"

data VerificationEmailConverterConf =
  VerificationEmailConverterConf
    { verificationEmailConverterConfFromAddress :: Maybe EmailAddress
    }
  deriving (Show, Eq)

instance FromJSON VerificationEmailConverterConf where
  parseJSON = viaYamlSchema

instance YamlSchema VerificationEmailConverterConf where
  yamlSchema =
    objectParser "VerificationEmailConverterConf" $
    VerificationEmailConverterConf <$>
    optionalField "from" "The from address for verification emails"

data TriggeredEmailConverterConf =
  TriggeredEmailConverterConf
    { triggeredEmailConverterConfFromAddress :: Maybe EmailAddress
    }
  deriving (Show, Eq)

instance FromJSON TriggeredEmailConverterConf where
  parseJSON = viaYamlSchema

instance YamlSchema TriggeredEmailConverterConf where
  yamlSchema =
    objectParser "TriggeredEmailConverterConf" $
    TriggeredEmailConverterConf <$>
    optionalField "from" "The from address for triggered item emails"

data AdminNotificationEmailConverterConf =
  AdminNotificationEmailConverterConf
    { adminNotificationEmailConverterConfFromAddress :: Maybe EmailAddress
    , adminNotificationEmailConverterConfToAddress :: Maybe EmailAddress
    }
  deriving (Show, Eq)

instance FromJSON AdminNotificationEmailConverterConf where
  parseJSON = viaYamlSchema

instance YamlSchema AdminNotificationEmailConverterConf where
  yamlSchema =
    objectParser "AdminNotificationEmailConverterConf" $
    AdminNotificationEmailConverterConf <$>
    optionalField "from" "The 'from' address for admin notification emails" <*>
    optionalField "to" "The 'to' address for admin notification emails"

data Environment =
  Environment
    { envConfigFile :: Maybe String
    , envDb :: Maybe Text
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
    , looperEnvDefaultRetryAmount :: Maybe Int
    , looperEnvTriggererEnv :: LooperEnvWith ()
    , looperEnvEmailerEnv :: LooperEnvWith ()
    , looperEnvTriggeredIntrayItemSchedulerEnv :: LooperEnvWith ()
    , looperEnvTriggeredIntrayItemSenderEnv :: LooperEnvWith ()
    , looperEnvVerificationEmailConverterEnv :: LooperEnvWith (Maybe EmailAddress)
    , looperEnvTriggeredEmailSchedulerEnv :: LooperEnvWith ()
    , looperEnvTriggeredEmailConverterEnv :: LooperEnvWith (Maybe EmailAddress)
    , looperEnvAdminNotificationEmailConverterEnv :: LooperEnvWith AdminNotificationEmailConverterEnvironment
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

data AdminNotificationEmailConverterEnvironment =
  AdminNotificationEmailConverterEnvironment
    { adminNotificationEmailConverterEnvFromAddress :: Maybe EmailAddress
    , adminNotificationEmailConverterEnvToAddress :: Maybe EmailAddress
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
    , serveSetFreeloaders :: ![Username]
    , serveSetMonetisationSettings :: !(Maybe MonetisationSettings)
    , serveSetLoopersSettings :: !LoopersSettings
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
    , stripeSetStripeConfig :: StripeConfig
    , stripeSetPublishableKey :: !Text
    }
  deriving (Show)

data LoopersSettings =
  LoopersSettings
    { looperSetTriggererSets :: LooperSetsWith ()
    , looperSetEmailerSets :: LooperSetsWith EmailerSettings
    , looperSetTriggeredIntrayItemSchedulerSets :: LooperSetsWith ()
    , looperSetTriggeredIntrayItemSenderSets :: LooperSetsWith ()
    , looperSetVerificationEmailConverterSets :: LooperSetsWith VerificationEmailConverterSettings
    , looperSetTriggeredEmailSchedulerSets :: LooperSetsWith ()
    , looperSetTriggeredEmailConverterSets :: LooperSetsWith TriggeredEmailConverterSettings
    , looperSetAdminNotificationEmailConverterSets :: LooperSetsWith AdminNotificationEmailConverterSettings
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

data AdminNotificationEmailConverterSettings =
  AdminNotificationEmailConverterSettings
    { adminNotificationEmailConverterSetFromAddress :: !EmailAddress
    , adminNotificationEmailConverterSetFromName :: !Text
    , adminNotificationEmailConverterSetToAddress :: !EmailAddress
    , adminNotificationEmailConverterSetToName :: !Text
    , adminNotificationEmailConverterSetWebHost :: !Text
    }
  deriving (Show)
