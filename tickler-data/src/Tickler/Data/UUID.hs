{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Tickler.Data.UUID
    (
    ) where

import Import

import qualified Data.ByteString.Lazy as LB
import qualified Data.Text as T
import qualified Data.UUID as UUID
import Data.UUID.Typed

import Database.Persist
import Database.Persist.Sql

instance PersistField (UUID a) where
    toPersistValue (UUID uuid) =
        PersistByteString $ LB.toStrict $ UUID.toByteString uuid
    fromPersistValue (PersistByteString bs) =
        case UUID.fromByteString $ LB.fromStrict bs of
            Nothing -> Left "Invalidy Bytestring to convert to UUID"
            Just uuid -> Right $ UUID uuid
    fromPersistValue pv =
        Left $ "Invalid Persist value to parse to UUID: " <> T.pack (show pv)

instance PersistFieldSql (UUID a) where
    sqlType Proxy = SqlBlob
