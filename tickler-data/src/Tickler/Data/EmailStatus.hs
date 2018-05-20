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
    = Unsent
    | Sending
    | Sent
    | Error
    deriving (Show, Read, Eq, Ord, Bounded, Enum, Generic)

instance Validity EmailStatus

instance PersistField EmailStatus where
    toPersistValue Unsent = PersistByteString "unsent"
    toPersistValue Sending = PersistByteString "sending"
    toPersistValue Sent = PersistByteString "sent"
    toPersistValue Error = PersistByteString "error"
    fromPersistValue (PersistByteString "unsent") = Right Unsent
    fromPersistValue (PersistByteString "sending") = Right Sending
    fromPersistValue (PersistByteString "sent") = Right Sent
    fromPersistValue (PersistByteString "error") = Right Error
    fromPersistValue _ = Left "Not a valid EmailStatus"

instance PersistFieldSql EmailStatus where
    sqlType Proxy = SqlString
