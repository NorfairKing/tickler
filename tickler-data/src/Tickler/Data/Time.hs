{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Tickler.Data.Time where

import Autodocodec
import Data.Aeson (FromJSON, ToJSON)
import Data.Time
import Database.Persist.Sql
import Import
import Text.Printf

instance HasCodec TimeZone where
  codec =
    object "TimeZone" $
      TimeZone
        <$> requiredField' "offset" .= timeZoneMinutes
        <*> requiredField' "summer" .= timeZoneSummerOnly
        <*> requiredField' "name" .= timeZoneName

deriving via (Autodocodec TimeZone) instance (FromJSON TimeZone)

deriving via (Autodocodec TimeZone) instance (ToJSON TimeZone)

instance PersistFieldSql TimeZone where
  sqlType Proxy = SqlString

instance PersistField TimeZone where
  fromPersistValue = fromPersistValueJSON
  toPersistValue = toPersistValueJSON

timeZoneChoices :: [TimeZone]
timeZoneChoices =
  map
    ( \i ->
        TimeZone
          { timeZoneMinutes = 60 * i,
            timeZoneSummerOnly = False,
            timeZoneName = concat $ "UTC" : [printf "%+d" i | i /= 0]
          }
    )
    [-12 .. 12]
