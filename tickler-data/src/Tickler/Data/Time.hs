{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Tickler.Data.Time where

import Import

import Data.Aeson
import Data.Time

import Database.Persist.Sql

instance ToJSON TimeZone where
    toJSON TimeZone {..} =
        object
            [ "offset" .= timeZoneMinutes
            , "summer" .= timeZoneSummerOnly
            , "name" .= timeZoneName
            ]

instance FromJSON TimeZone where
    parseJSON =
        withObject "TimeZone" $ \o ->
            TimeZone <$> o .: "offset" <*> o .: "summer" <*> o .: "name"

instance PersistFieldSql TimeZone where
    sqlType Proxy = SqlString

instance PersistField TimeZone where
    fromPersistValue = fromPersistValueJSON
    toPersistValue = toPersistValueJSON

timezoneChoices :: [TimeZone]
timezoneChoices = map hoursToTimeZone [-12 .. 12]
