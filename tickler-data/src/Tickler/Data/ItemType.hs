{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

module Tickler.Data.ItemType where

import Data.Aeson
import Database.Persist
import Database.Persist.Sql
import Import

data ItemType
  = TextItem
  deriving (Show, Read, Eq, Ord, Bounded, Enum, Generic)

instance Validity ItemType

instance PersistField ItemType where
  toPersistValue TextItem = PersistByteString "text"
  fromPersistValue (PersistByteString "text") = Right TextItem
  fromPersistValue _ = Left "Not a valid ItemType"

instance PersistFieldSql ItemType where
  sqlType Proxy = SqlString

instance FromJSON ItemType where
  parseJSON (String "text") = pure TextItem
  parseJSON _ = fail "Not a valid TextItem"

instance ToJSON ItemType where
  toJSON TextItem = String "text"
