{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

module Tickler.Data.EmailStatus where

import Database.Persist
import Database.Persist.Sql
import Import

data EmailStatus
  = EmailUnsent
  | EmailSent
  | EmailError
  deriving (Show, Read, Eq, Bounded, Enum, Generic)

instance Validity EmailStatus

instance PersistField EmailStatus where
  toPersistValue =
    toPersistValue . \case
      EmailUnsent -> "unsent"
      EmailSent -> "sent"
      EmailError -> ("error" :: ByteString)
  fromPersistValue pv = do
    sb <- fromPersistValue pv
    case sb :: ByteString of
      "unsent" -> Right EmailUnsent
      "sending" -> Right EmailUnsent
      "sent" -> Right EmailSent
      "error" -> Right EmailError
      _ -> Left "Unknown EmailStatus"

instance PersistFieldSql EmailStatus where
  sqlType Proxy = SqlString
