{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

module Tickler.Data.ItemType where

import Autodocodec
import Data.Aeson (FromJSON, ToJSON)
import qualified Data.Text as T
import Database.Persist
import Database.Persist.Sql
import Import

data ItemType
  = TextItem
  deriving stock (Show, Read, Eq, Ord, Bounded, Enum, Generic)
  deriving (FromJSON, ToJSON) via (Autodocodec ItemType)

instance Validity ItemType

instance PersistField ItemType where
  toPersistValue = toPersistValue . renderItemType
  fromPersistValue = fromPersistValue >=> (left T.pack . parseItemType)

instance PersistFieldSql ItemType where
  sqlType Proxy = SqlString

instance HasCodec ItemType where
  codec = bimapCodec parseItemType renderItemType codec

renderItemType :: ItemType -> Text
renderItemType = \case
  TextItem -> "text"

parseItemType :: Text -> Either String ItemType
parseItemType = \case
  "text" -> Right TextItem
  t -> Left $ "Unknown ItemType: " <> show t
