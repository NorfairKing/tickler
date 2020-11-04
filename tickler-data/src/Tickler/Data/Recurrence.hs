{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Tickler.Data.Recurrence where

import Control.Applicative
import Data.Aeson
import Data.Time
import Data.Word
import Database.Persist
import Database.Persist.Sql
import Import

data Recurrence
  = EveryDaysAtTime Word (Maybe TimeOfDay)
  | EveryMonthsOnDay Word (Maybe Word8) (Maybe TimeOfDay)
  deriving (Show, Eq, Ord, Generic)

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

instance FromJSON Recurrence where
  parseJSON v =
    withObject
      "Recurrence"
      ( \o ->
          o .: "every-x-days"
            >>= withObject
              "EveryDaysAtTime"
              (\o' -> EveryDaysAtTime <$> o' .: "days" <*> o' .: "time-of-day")
      )
      v
      <|> withObject
        "Recurrence"
        ( \o ->
            o .: "every-x-months"
              >>= withObject
                "EveryMonthsOnDay"
                (\o' -> EveryMonthsOnDay <$> o' .: "months" <*> o' .:? "day" <*> o' .:? "time-of-day")
        )
        v

instance ToJSON Recurrence where
  toJSON (EveryDaysAtTime ds tod) =
    object ["every-x-days" .= object ["days" .= ds, "time-of-day" .= tod]]
  toJSON (EveryMonthsOnDay ms md mtod) =
    object ["every-x-months" .= object ["months" .= ms, "day" .= md, "time-of-day" .= mtod]]

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
