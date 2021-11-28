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

data Recurrence
  = EveryDaysAtTime Word (Maybe TimeOfDay)
  | EveryMonthsOnDay Word (Maybe Word8) (Maybe TimeOfDay)
  deriving stock (Show, Eq, Ord, Generic)
  deriving (FromJSON, ToJSON) via (Autodocodec Recurrence)

instance Validity Recurrence where
  validate (EveryDaysAtTime ds mtod) =
    decorate "EveryDaysAtTime" $
      mconcat
        [ delve "Word" ds,
          delve "Maybe TimeOfDay" mtod,
          declare "The number of days is strictly positive" $ ds >= 1
        ]
  validate (EveryMonthsOnDay ms md mtod) =
    decorate "EveryMonthsOnDay" $
      mconcat
        [ delve "Word" ms,
          delve "Maybe Word8" md,
          delve "Maybe TimeOfDay" mtod,
          declare "The number of months is strictly positive" $ ms >= 1,
          declare "The day of the month is strictly positive" $ maybe True (>= 1) md,
          declare "The day of the month is less than or equal to 31" $ maybe True (<= 31) md
        ]

instance HasCodec Recurrence where
  codec =
    object "Recurrence" $
      dimapCodec f g $ eitherCodec everyDaysAtTimeCodec everyMonthsOnDayCodec
    where
      f = \case
        Left (ds, mtod) -> EveryDaysAtTime ds mtod
        Right (ms, md, mtod) -> EveryMonthsOnDay ms md mtod
      g = \case
        EveryDaysAtTime ds mtod -> Left (ds, mtod)
        EveryMonthsOnDay ms md mtod -> Right (ms, md, mtod)
      everyDaysAtTimeCodec :: JSONObjectCodec (Word, Maybe TimeOfDay)
      everyDaysAtTimeCodec =
        requiredFieldWith
          "every-x-days"
          ( object "EveryDaysAtTime" $
              (,)
                <$> requiredField "days" "days between recurrence" .= fst
                <*> optionalField "time-of-day" "time of day within the recurring day" .= snd
          )
          "every x days"
      everyMonthsOnDayCodec :: JSONObjectCodec (Word, Maybe Word8, Maybe TimeOfDay)
      everyMonthsOnDayCodec =
        requiredFieldWith
          "every-x-months"
          ( object "EveryMonthsOnDay" $
              (,,)
                <$> requiredField "months" "months between recurrence" .= (\(a, _, _) -> a)
                <*> optionalField "day" "day within the recurring month" .= (\(_, a, _) -> a)
                <*> optionalField "time-of-day" "time of day within the recurring day" .= (\(_, _, a) -> a)
          )
          "every x months"

instance PersistField Recurrence where
  fromPersistValue = fromPersistValueJSON
  toPersistValue = toPersistValueJSON

instance PersistFieldSql Recurrence where
  sqlType Proxy = SqlString

everyDay :: Recurrence
everyDay = EveryDaysAtTime 1 Nothing

everyDaysAtTime :: Word -> Maybe TimeOfDay -> Maybe Recurrence
everyDaysAtTime ds mtod = constructValid $ EveryDaysAtTime ds mtod

everyMonthsOnDayAtTime :: Word -> Maybe Word8 -> Maybe TimeOfDay -> Maybe Recurrence
everyMonthsOnDayAtTime ms md mtod = constructValid $ EveryMonthsOnDay ms md mtod

nextScheduledTime :: Day -> Maybe TimeOfDay -> Recurrence -> (Day, Maybe TimeOfDay)
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
