{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}

module Tickler.Web.Server.Handler.Add
    ( getAddR
    , postAddR
    ) where

import Import

import qualified Data.Text as T
import Data.Time
import Data.Word

import Yesod

import Tickler.Client

import Tickler.Web.Server.Foundation

getAddR :: Handler Html
getAddR =
    withLogin $ \_ -> do
        token <- genToken
        withNavBar $(widgetFile "add")

data NewItem = NewItem
    { newItemText :: Textarea
    , newItemScheduledDay :: Day
    , newItemScheduledTime :: Maybe TimeOfDay
    , newItemRecurrenceData :: RecurrenceData
    }

data RecurrenceData = RecurrenceData
    { recurrenceDataOption :: RecurrenceOption
    , recurrenceDataDays :: Maybe Word
    , recurrenceDataDayTimeOfDay :: Maybe TimeOfDay
    , recurrenceDataMonths :: Maybe Word
    , recurrenceDataMonthDay :: Maybe Word8
    , recurrenceDataMonthTimeOfDay :: Maybe TimeOfDay
    }

newItemForm :: FormInput Handler NewItem
newItemForm =
    NewItem <$> ireq textareaField "contents" <*> ireq dayField "scheduled-day" <*>
    iopt timeField "scheduled-time" <*>
    recurrenceDataForm

recurrenceDataForm :: FormInput Handler RecurrenceData
recurrenceDataForm =
    RecurrenceData <$>
    ireq
        (radioField $
         pure $
         mkOptionList $
         map
             (\v -> Option (T.pack $ show v) v (T.pack $ show v))
             [minBound .. maxBound])
        "recurrence" <*>
    iopt
        (checkMMap
             (pure . (pure :: a -> Either Text a) . fromInteger)
             fromIntegral
             intField)
        "days" <*>
    iopt timeField "day-time-of-day" <*>
    iopt
        (checkMMap
             (pure . (pure :: a -> Either Text a) . fromInteger)
             fromIntegral
             intField)
        "months" <*>
    iopt
        (checkMMap
             (pure . (pure :: a -> Either Text a) . fromInteger)
             fromIntegral
             intField)
        "day" <*>
    iopt timeField "month-time-of-day"

data RecurrenceOption
    = NoRecurrence
    | Days
    | Months
    deriving (Show, Read, Eq, Enum, Bounded)

mkRecurrence :: RecurrenceData -> Handler (Maybe Recurrence)
mkRecurrence RecurrenceData {..} =
    case recurrenceDataOption of
        NoRecurrence -> pure Nothing
        Days ->
            case everyDaysAtTime
                     (fromMaybe 1 recurrenceDataDays)
                     recurrenceDataDayTimeOfDay of
                Nothing -> invalidArgs ["Invalid recurrence"]
                Just r -> pure $ Just r
        Months ->
            case everyMonthsOnDayAtTime
                     (fromMaybe 1 recurrenceDataDays)
                     recurrenceDataMonthDay
                     recurrenceDataMonthTimeOfDay of
                Nothing -> invalidArgs ["Invalid recurrence"]
                Just r -> pure $ Just r

postAddR :: Handler Html
postAddR =
    withLogin $ \t -> do
        AccountSettings {..} <- runClientOrErr $ clientGetAccountSettings t
        NewItem {..} <- runInputPost newItemForm
        recurrence <- mkRecurrence newItemRecurrenceData
        void $
            runClientOrErr $
            clientPostAddItem t $
            Tickle
                { tickleContent = textTypedItem $ unTextarea newItemText
                , tickleScheduledDay = newItemScheduledDay
                , tickleScheduledTime = newItemScheduledTime
                , tickleRecurrence = recurrence
                }
        redirect AddR
