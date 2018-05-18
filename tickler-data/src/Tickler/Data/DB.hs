{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuasiQuotes #-}
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

TicklerItem
    identifier ItemUUID
    type ItemType
    contents ByteString
    timestamp UTCTime
    userId AccountUUID
    UniqueItem identifier type contents timestamp userId
    UniqueIdentifier identifier userId
    deriving Show
    deriving Eq
    deriving Generic
|]

instance Validity TicklerItem

instance Validity User
