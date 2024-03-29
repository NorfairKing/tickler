{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Tickler.Data.Recurrence where

import Autodocodec
import Control.Applicative
import Data.Aeson (FromJSON, ToJSON)
import Data.Time
import Data.Word
import Database.Persist
import Database.Persist.Sql
import Import
import Tickler.Data.MinuteOfDay

data Recurrence
  = EveryDaysAtTime Word (Maybe MinuteOfDay)
  | EveryMonthsOnDay Word (Maybe Word8) (Maybe MinuteOfDay)
  deriving stock (Show, Eq, Generic)
  deriving (FromJSON, ToJSON) via (Autodocodec Recurrence)

instance Validity Recurrence where
  validate (EveryDaysAtTime ds mtod) =
    decorate "EveryDaysAtTime"
      $ mconcat
        [ delve "Word" ds,
          delve "Maybe TimeOfDay" mtod,
          declare "The number of days is strictly positive" $ ds >= 1
        ]
  validate (EveryMonthsOnDay ms md mtod) =
    decorate "EveryMonthsOnDay"
      $ mconcat
        [ delve "Word" ms,
          delve "Maybe Word8" md,
          delve "Maybe TimeOfDay" mtod,
          declare "The number of months is strictly positive" $ ms >= 1,
          declare "The day of the month is strictly positive" $ maybe True (>= 1) md,
          declare "The day of the month is less than or equal to 31" $ maybe True (<= 31) md
        ]

instance HasCodec Recurrence where
  codec =
    object "Recurrence"
      $ dimapCodec f g
      $ eitherCodec everyDaysAtTimeCodec everyMonthsOnDayCodec
    where
      f = \case
        Left (ds, mtod) -> EveryDaysAtTime ds mtod
        Right (ms, md, mtod) -> EveryMonthsOnDay ms md mtod
      g = \case
        EveryDaysAtTime ds mtod -> Left (ds, mtod)
        EveryMonthsOnDay ms md mtod -> Right (ms, md, mtod)
      everyDaysAtTimeCodec :: JSONObjectCodec (Word, Maybe MinuteOfDay)
      everyDaysAtTimeCodec =
        requiredFieldWith
          "every-x-days"
          ( object "EveryDaysAtTime"
              $ (,)
              <$> requiredField "days" "days between recurrence"
                .= fst
              <*> optionalFieldOrNull "time-of-day" "time of day within the recurring day"
                .= snd
          )
          "every x days"
      everyMonthsOnDayCodec :: JSONObjectCodec (Word, Maybe Word8, Maybe MinuteOfDay)
      everyMonthsOnDayCodec =
        requiredFieldWith
          "every-x-months"
          ( object "EveryMonthsOnDay"
              $ (,,)
              <$> requiredField "months" "months between recurrence"
                .= (\(a, _, _) -> a)
              <*> optionalFieldOrNull "day" "day within the recurring month"
                .= (\(_, a, _) -> a)
              <*> optionalFieldOrNull "time-of-day" "time of day within the recurring day"
                .= (\(_, _, a) -> a)
          )
          "every x months"

instance PersistField Recurrence where
  fromPersistValue = fromPersistValueJSON
  toPersistValue = toPersistValueJSON

instance PersistFieldSql Recurrence where
  sqlType Proxy = SqlString

everyDay :: Recurrence
everyDay = EveryDaysAtTime 1 Nothing

everyDaysAtTime :: Word -> Maybe MinuteOfDay -> Maybe Recurrence
everyDaysAtTime ds mtod = constructValid $ EveryDaysAtTime ds mtod

everyMonthsOnDayAtTime :: Word -> Maybe Word8 -> Maybe MinuteOfDay -> Maybe Recurrence
everyMonthsOnDayAtTime ms md mtod = constructValid $ EveryMonthsOnDay ms md mtod

nextScheduledTime :: Day -> Maybe MinuteOfDay -> Recurrence -> (Day, Maybe MinuteOfDay)
nextScheduledTime scheduledDay _ r =
  case r of
    EveryDaysAtTime ds mtod -> (addDays (fromIntegral ds) scheduledDay, mtod)
    EveryMonthsOnDay ms md mtod ->
      let clipped = addGregorianMonthsClip (fromIntegral ms) scheduledDay
          day =
            case md of
              Nothing -> clipped
              Just d_ ->
                let (y, m, _) = toGregorian clipped
                 in fromGregorian y m (fromIntegral d_)
       in (day, mtod)
