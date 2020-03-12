{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

module Tickler.Data.EmailStatus where

import Import

import Database.Persist
import Database.Persist.Sql

data EmailStatus
  = EmailUnsent
  | EmailSent
  | EmailError
  deriving (Show, Read, Eq, Ord, Bounded, Enum, Generic)

instance Validity EmailStatus

instance PersistField EmailStatus where
  toPersistValue EmailUnsent = PersistByteString "unsent"
  toPersistValue EmailSent = PersistByteString "sent"
  toPersistValue EmailError = PersistByteString "error"
  fromPersistValue (PersistByteString "unsent") = Right EmailUnsent
  fromPersistValue (PersistByteString "sending") = Right EmailUnsent
  fromPersistValue (PersistByteString "sent") = Right EmailSent
  fromPersistValue (PersistByteString "error") = Right EmailError
  fromPersistValue _ = Left "Not a valid EmailStatus"

instance PersistFieldSql EmailStatus where
  sqlType Proxy = SqlString
