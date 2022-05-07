{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

module Tickler.Data.TriggerType where

import qualified Data.Text as T
import Database.Persist
import Database.Persist.Sql
import Import

data TriggerType
  = EmailTriggerType
  | IntrayTriggerType
  deriving stock (Show, Read, Eq, Bounded, Enum, Generic)

instance Validity TriggerType

instance PersistField TriggerType where
  toPersistValue = toPersistValue . renderTriggerType
  fromPersistValue = fromPersistValue >=> (left T.pack . parseTriggerType)

instance PersistFieldSql TriggerType where
  sqlType Proxy = SqlString

renderTriggerType :: TriggerType -> ByteString
renderTriggerType = \case
  EmailTriggerType -> "email"
  IntrayTriggerType -> "intray"

parseTriggerType :: ByteString -> Either String TriggerType
parseTriggerType = \case
  "email" -> Right EmailTriggerType
  "intray" -> Right IntrayTriggerType
  t -> Left $ "Unknown trigger type: " <> show t
