{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Tickler.Data.DB where

import Data.Mergeful.Persistent ()
import Data.Mergeful.Timed as Mergeful
import Data.Time
import Database.Persist.Sql
import Database.Persist.TH
import Import
import qualified Intray.Data as Intray
import Tickler.Data.AccountUUID
import Tickler.Data.EmailAddress
import Tickler.Data.EmailStatus
import Tickler.Data.EmailVerificationKey
import Tickler.Data.HashedPassword
import Tickler.Data.ItemType
import Tickler.Data.ItemUUID
import Tickler.Data.Recurrence
import Tickler.Data.Stripe ()
import Tickler.Data.Time ()
import Tickler.Data.TriggerType
import Tickler.Data.TriggerUUID
import Tickler.Data.Url
import Tickler.Data.Username
import qualified Web.Stripe.Types as Stripe

share
  [mkPersist sqlSettings, mkDeleteCascade sqlSettings, mkMigrate "migrateAll"]
  [persistLowerCase|

User
    identifier AccountUUID
    username Username
    hashedPassword HashedPassword
    created UTCTime
    lastLogin UTCTime Maybe

    UniqueUserIdentifier identifier
    UniqueUsername username

    deriving Show
    deriving Eq
    deriving Ord
    deriving Generic
    deriving Typeable

Customer
    user AccountUUID
    stripeCustomer Stripe.CustomerId
    UniqueCustomerUser user
    UniqueUserCustomer stripeCustomer
    deriving Show
    deriving Eq
    deriving Generic


StripeEvent
    event Stripe.EventId
    error Text Maybe
    UniqueStripeEvent event
    deriving Show
    deriving Eq
    deriving Generic


UserSettings
    userId AccountUUID
    timeZone TimeZone

    UniqueUserSettings userId

    deriving Show
    deriving Eq
    deriving Ord
    deriving Generic
    deriving Typeable


TicklerItem
    identifier ItemUUID
    userId AccountUUID
    type ItemType
    contents ByteString
    created UTCTime
    serverTime Mergeful.ServerTime default=0

    scheduledDay Day
    scheduledTime TimeOfDay Maybe

    recurrence Recurrence Maybe

    UniqueItemIdentifier identifier

    deriving Show
    deriving Eq
    deriving Ord
    deriving Generic
    deriving Typeable


TriggeredItem
    identifier ItemUUID
    userId AccountUUID
    type ItemType
    contents ByteString
    created UTCTime

    scheduledDay Day
    scheduledTime TimeOfDay Maybe

    recurrence Recurrence Maybe

    triggered UTCTime

    UniqueTriggeredItemIdentifier identifier

    deriving Show
    deriving Eq
    deriving Ord
    deriving Generic
    deriving Typeable


UserTrigger
    userId AccountUUID
    triggerType TriggerType
    triggerId TriggerUUID

    deriving Show
    deriving Eq
    deriving Ord
    deriving Generic
    deriving Typeable


IntrayTrigger
    identifier TriggerUUID
    url BaseUrl
    username Intray.Username
    accessKey Intray.AccessKeySecret
    added UTCTime

    UniqueIntrayTrigger identifier

    deriving Show
    deriving Eq
    deriving Ord
    deriving Generic
    deriving Typeable


EmailTrigger
    identifier TriggerUUID
    address EmailAddress
    verificationKey EmailVerificationKey
    verified Bool
    added UTCTime

    UniqueEmailTrigger identifier

    deriving Show
    deriving Eq
    deriving Ord
    deriving Generic
    deriving Typeable


VerificationEmail
    to          EmailAddress
    key         EmailVerificationKey
    trigger     TriggerUUID
    scheduled   UTCTime
    email       EmailId      Maybe

    deriving Show
    deriving Eq
    deriving Ord
    deriving Generic
    deriving Typeable


AdminNotificationEmail
    contents    Text
    email       EmailId      Maybe

    deriving Show
    deriving Eq
    deriving Ord
    deriving Generic
    deriving Typeable

TriggeredIntrayItem
    item            ItemUUID
    trigger         TriggerUUID
    intrayItemUUID  Intray.ItemUUID        Maybe
    error           Text                   Maybe

    UniqueTriggeredIntrayItem item trigger

    deriving Show
    deriving Eq
    deriving Ord
    deriving Generic
    deriving Typeable


TriggeredEmail
    item            ItemUUID
    trigger         TriggerUUID
    email           EmailId     Maybe
    error           Text                   Maybe

    UniqueTriggeredEmail item trigger

    deriving Show
    deriving Eq
    deriving Ord
    deriving Generic
    deriving Typeable


Email
    to              EmailAddress
    from            EmailAddress
    fromName        Text
    subject         Text
    textContent     Text
    htmlContent     Text
    status          EmailStatus
    sendError       Text         Maybe
    sesId           Text         Maybe
    scheduled       UTCTime
    sendAttempt     UTCTime      Maybe

    deriving Show
    deriving Eq
    deriving Ord
    deriving Generic
    deriving Typeable
|]

instance Validity User

instance Validity UserSettings

instance Validity TicklerItem

instance Validity TriggeredItem

instance Validity UserTrigger

instance Validity IntrayTrigger

instance Validity EmailTrigger

instance Validity TriggeredIntrayItem

instance Validity VerificationEmail

instance Validity TriggeredEmail

instance Validity Email

instance Validity (Key a) where
  validate = trivialValidation
