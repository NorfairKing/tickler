{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

module Tickler.Data.TriggerType where

import Import

import Data.Aeson

import Database.Persist
import Database.Persist.Sql

data TriggerType
  = EmailTriggerType
  | IntrayTriggerType
  deriving (Show, Read, Eq, Ord, Bounded, Enum, Generic)

instance Validity TriggerType

instance PersistField TriggerType where
  toPersistValue EmailTriggerType = PersistByteString "email"
  toPersistValue IntrayTriggerType = PersistByteString "intray"
  fromPersistValue (PersistByteString "email") = Right EmailTriggerType
  fromPersistValue (PersistByteString "intray") = Right IntrayTriggerType
  fromPersistValue _ = Left "Not a valid TriggerType"

instance PersistFieldSql TriggerType where
  sqlType Proxy = SqlString

instance FromJSON TriggerType where
  parseJSON (String "email") = pure EmailTriggerType
  parseJSON (String "intray") = pure IntrayTriggerType
  parseJSON _ = fail "Not a valid TriggerType"

instance ToJSON TriggerType where
  toJSON EmailTriggerType = String "email"
  toJSON IntrayTriggerType = String "intray"
