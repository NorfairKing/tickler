{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Tickler.Data.Recurrence where

import Import

import Control.Applicative

import Data.Aeson
import Data.Time
import Data.Word

import Database.Persist
import Database.Persist.Sql

data Recurrence
    = Nominal NominalDiffTime
    | EveryDaysAtTime Word
                      (Maybe TimeOfDay)
    | EveryMonthsOnDay Word
                       (Maybe Word8)
                       (Maybe TimeOfDay)
    deriving (Show, Eq, Ord, Generic)

nominal :: NominalDiffTime -> Maybe Recurrence
nominal = constructValid . Nominal

everyDaysAtTime :: Word -> TimeOfDay -> Maybe Recurrence
everyDaysAtTime w tod = constructValid $ EveryDaysAtTime w $ Just tod

everyDayAtTime :: TimeOfDay -> Maybe Recurrence
everyDayAtTime = constructValid . EveryDaysAtTime 1 . Just

everyDay :: Recurrence
everyDay = EveryDaysAtTime 1 Nothing

instance Validity Recurrence where
    validate (Nominal ndt) =
        decorate "Nominal" $
        mconcat
            [ delve "NominalDiffTime" ndt
            , declare "the nominal time is positive" $ ndt >= 0
            ]
    validate (EveryDaysAtTime ds mtod) =
        decorate "EveryDaysAtTime" $
        mconcat [delve "Word" ds, delve "Maybe TimeOfDay" mtod]
    validate (EveryMonthsOnDay ms md mtod) =
        decorate "EveryMonthsOnDay" $
        mconcat
            [ delve "Word" ms
            , delve "Maybe Word8" md
            , delve "Maybe TimeOfDay" mtod, declare "The day of the month is positive" $ maybe True (>=0) md
            ]

instance FromJSON Recurrence where
    parseJSON v =
        (Nominal <$> parseJSON v) <|>
        withObject
            "Recurrence"
            (\o ->
                 o .: "every-x-days" >>=
                 withObject
                     "EveryDaysAtTime"
                     (\o' ->
                          EveryDaysAtTime <$> o' .: "days" <*>
                          o' .: "time-of-day"))
            v <|>
        withObject
            "Recurrence"
            (\o ->
                 o .: "every-x-months" >>=
                 withObject
                     "EveryMonthsOnDay"
                     (\o' ->
                          EveryMonthsOnDay <$> o' .: "months" <*> o' .:? "day" <*>
                          o' .:? "time-of-day"))
            v

instance ToJSON Recurrence where
    toJSON (Nominal ndt) = toJSON ndt
    toJSON (EveryDaysAtTime ds tod) =
        object ["every-x-days" .= object ["days" .= ds, "time-of-day" .= tod]]
    toJSON (EveryMonthsOnDay ms md mtod) =
        object
            [ "every-x-months" .=
              object ["months" .= ms, "day" .= md, "time-of-day" .= mtod]
            ]

instance PersistField Recurrence where
    fromPersistValue = fromPersistValueJSON
    toPersistValue = toPersistValueJSON

instance PersistFieldSql Recurrence where
    sqlType Proxy = SqlString
