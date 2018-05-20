{-# LANGUAGE DeriveGeneric #-}
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

import Import

import Data.Time

import Database.Persist.Sql
import Database.Persist.TH

import Tickler.Data.AccountUUID
import Tickler.Data.HashedPassword
import Tickler.Data.ItemType
import Tickler.Data.ItemUUID
import Tickler.Data.Time ()
import Tickler.Data.Username

share [mkPersist sqlSettings, mkMigrate "migrateAll"]
    [persistLowerCase|

User
    identifier AccountUUID
    username Username
    hashedPassword HashedPassword
    createdTimestamp UTCTime
    lastLogin UTCTime Maybe
    UniqueUserIdentifier identifier
    UniqueUsername username
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

TicklerItem
    identifier ItemUUID
    userId AccountUUID
    type ItemType
    contents ByteString
    created UTCTime
    scheduled UTCTime
    UniqueItem identifier userId type contents
    UniqueIdentifier identifier userId
    deriving Show
    deriving Eq
    deriving Generic

TriggeredItem
    identifier ItemUUID
    userId AccountUUID
    type ItemType
    contents ByteString
    created UTCTime
    scheduled UTCTime
    UniqueTriggeredItem identifier userId type contents
    UniqueTriggeredIdentifier identifier userId
    deriving Show
    deriving Eq
    deriving Generic
|]

instance Validity User

instance Validity UserSettings

instance Validity TicklerItem
