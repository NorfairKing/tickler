{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Tickler.Data.DB where

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
import Tickler.Data.ItemUUID
import Tickler.Data.MinuteOfDay
import Tickler.Data.Recurrence
import Tickler.Data.Time ()
import Tickler.Data.TriggerUUID
import Tickler.Data.Url
import Tickler.Data.Username

share
  [mkPersist sqlSettings, mkMigrate "serverAutoMigration"]
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
    deriving Generic
    deriving Typeable


StripeCustomer sql=customer
  user AccountUUID
  customer Text sql=stripe_customer -- Stripe customer id
  UniqueStripeCustomer user customer

  deriving Show
  deriving Eq
  deriving Generic


Subscription
  user AccountUUID
  end UTCTime

  UniqueSubscriptionUser user

  deriving Show
  deriving Eq
  deriving Generic

UserSettings
    userId AccountUUID
    timeZone TimeZone

    UniqueUserSettings userId

    deriving Show
    deriving Eq
    deriving Generic
    deriving Typeable


TicklerItem
    identifier ItemUUID
    userId AccountUUID
    contents Text
    created UTCTime

    scheduledDay Day
    scheduledTime MinuteOfDay Maybe

    recurrence Recurrence Maybe

    UniqueItemIdentifier userId identifier

    deriving Show
    deriving Eq
    deriving Generic
    deriving Typeable


TriggeredItem
    identifier ItemUUID
    userId AccountUUID
    contents Text
    created UTCTime

    scheduledDay Day
    scheduledTime MinuteOfDay Maybe

    recurrence Recurrence Maybe

    triggered UTCTime

    UniqueTriggeredItemIdentifier identifier

    deriving Show
    deriving Eq
    deriving Generic
    deriving Typeable

IntrayTrigger
    user AccountUUID
    identifier TriggerUUID
    url BaseUrl
    username Intray.Username
    accessKey Intray.AccessKeySecret
    added UTCTime

    UniqueIntrayTrigger identifier

    deriving Show
    deriving Eq
    deriving Generic
    deriving Typeable


EmailTrigger
    user AccountUUID
    identifier TriggerUUID
    address EmailAddress
    verificationKey EmailVerificationKey
    verified Bool
    added UTCTime

    UniqueEmailTrigger identifier

    deriving Show
    deriving Eq
    deriving Generic
    deriving Typeable


VerificationEmail
    to EmailAddress
    key EmailVerificationKey
    trigger TriggerUUID
    scheduled UTCTime
    email EmailId Maybe

    deriving Show
    deriving Eq
    deriving Generic
    deriving Typeable


AdminNotificationEmail
    contents Text
    email EmailId Maybe

    deriving Show
    deriving Eq
    deriving Generic
    deriving Typeable

TriggeredIntrayItem
    item ItemUUID
    trigger TriggerUUID
    intrayItemUUID Intray.ItemUUID Maybe
    error Text Maybe

    UniqueTriggeredIntrayItem item trigger

    deriving Show
    deriving Eq
    deriving Generic
    deriving Typeable


TriggeredEmail
    item ItemUUID
    trigger TriggerUUID
    email EmailId Maybe
    error Text Maybe

    UniqueTriggeredEmail item trigger

    deriving Show
    deriving Eq
    deriving Generic
    deriving Typeable


Email
    to EmailAddress
    from EmailAddress
    fromName Text
    subject Text
    textContent Text
    htmlContent Text
    status EmailStatus
    sendError Text Maybe
    sesId Text Maybe
    scheduled UTCTime
    sendAttempt UTCTime Maybe

    deriving Show
    deriving Eq
    deriving Generic
    deriving Typeable
|]

instance Validity User

instance Validity UserSettings

instance Validity TicklerItem

instance Validity TriggeredItem

instance Validity IntrayTrigger

instance Validity EmailTrigger

instance Validity TriggeredIntrayItem

instance Validity VerificationEmail

instance Validity TriggeredEmail

instance Validity Email

instance Validity (Key a) where
  validate = trivialValidation
