{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Tickler.Data.MinuteOfDay
  ( MinuteOfDay (..),
    minuteOfDayToTimeOfDay,
    timeOfDayToMinuteOfDay,
  )
where

import Autodocodec
import Data.Aeson (FromJSON, ToJSON)
import Data.Proxy
import Data.Time
import Data.Time.Format.Internal
import Data.Validity
import Data.Word
import Database.Persist.Sql
import Import

newtype MinuteOfDay = MinuteOfDay {minuteOfDay :: Word16}
  deriving stock (Eq, Ord, Generic)
  deriving newtype (Show, Read)
  deriving (FromJSON, ToJSON) via (Autodocodec MinuteOfDay)

instance Validity MinuteOfDay where
  validate m@MinuteOfDay {..} =
    mconcat
      [ genericValidate m,
        declare "The minute is smaller than 24*60" $ minuteOfDay < 24 * 60
      ]

instance HasCodec MinuteOfDay where
  codec = dimapCodec timeOfDayToMinuteOfDay minuteOfDayToTimeOfDay codec

instance PersistField MinuteOfDay where
  toPersistValue = toPersistValue . minuteOfDayToTimeOfDay
  fromPersistValue = fmap timeOfDayToMinuteOfDay . fromPersistValue

instance PersistFieldSql MinuteOfDay where
  sqlType Proxy = sqlType (Proxy :: Proxy TimeOfDay)

-- Format as if it were a TimeOfDay.
instance FormatTime MinuteOfDay where
  formatCharacter b c = do
    func <- formatCharacter b c
    pure $ \fo m -> func fo (minuteOfDayToTimeOfDay m)

-- Parse as if it were a TimeOfDay.
instance ParseTime MinuteOfDay where
  substituteTimeSpecifier Proxy =
    substituteTimeSpecifier (Proxy :: Proxy TimeOfDay)
  parseTimeSpecifier Proxy =
    parseTimeSpecifier (Proxy :: Proxy TimeOfDay)
  buildTime locale chars = timeOfDayToMinuteOfDay <$> buildTime locale chars

timeOfDayToMinuteOfDay :: TimeOfDay -> MinuteOfDay
timeOfDayToMinuteOfDay (TimeOfDay hs ms _) =
  MinuteOfDay $ fromIntegral $ hs * 60 + ms

minuteOfDayToTimeOfDay :: MinuteOfDay -> TimeOfDay
minuteOfDayToTimeOfDay (MinuteOfDay m) =
  let m' = fromIntegral m
      hs = m' `div` 60
      ms = m' - hs * 60
   in TimeOfDay hs ms 0
