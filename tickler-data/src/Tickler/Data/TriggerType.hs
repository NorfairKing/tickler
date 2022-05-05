{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

module Tickler.Data.TriggerType where

import Autodocodec
import Data.Aeson (FromJSON, ToJSON)
import qualified Data.Text as T
import Database.Persist
import Database.Persist.Sql
import Import

data TriggerType
  = EmailTriggerType
  | IntrayTriggerType
  deriving stock (Show, Read, Eq, Bounded, Enum, Generic)
  deriving (FromJSON, ToJSON) via (Autodocodec TriggerType)

instance Validity TriggerType

instance PersistField TriggerType where
  toPersistValue = toPersistValue . renderTriggerType
  fromPersistValue = fromPersistValue >=> (left T.pack . parseTriggerType)

instance PersistFieldSql TriggerType where
  sqlType Proxy = SqlString

instance HasCodec TriggerType where
  codec = bimapCodec parseTriggerType renderTriggerType codec

renderTriggerType :: TriggerType -> Text
renderTriggerType = \case
  EmailTriggerType -> "email"
  IntrayTriggerType -> "intray"

parseTriggerType :: Text -> Either String TriggerType
parseTriggerType = \case
  "email" -> Right EmailTriggerType
  "intray" -> Right IntrayTriggerType
  t -> Left $ "Unknown trigger type: " <> show t
