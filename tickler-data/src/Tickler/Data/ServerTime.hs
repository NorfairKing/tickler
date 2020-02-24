{-# OPTIONS_GHC -fno-warn-orphans #-}

module Tickler.Data.ServerTime where

import Import

import Data.Mergeful.Timed as Mergeful
import Data.Word

import Database.Persist.Sql

instance PersistFieldSql Mergeful.ServerTime where
  sqlType Proxy = sqlType (Proxy :: Proxy Word64)

instance PersistField Mergeful.ServerTime where
  fromPersistValue = fmap ServerTime . fromPersistValue
  toPersistValue = toPersistValue . unServerTime
