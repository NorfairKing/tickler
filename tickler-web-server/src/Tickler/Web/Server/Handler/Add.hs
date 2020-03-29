{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}

module Tickler.Web.Server.Handler.Add where

import Import

import qualified Data.Text as T
import Data.Time
import Data.Word

import Yesod

import qualified Network.HTTP.Types as Http

import Tickler.Client

import Tickler.Web.Server.Foundation

getAddR :: Handler Html
getAddR =
  withLogin $ \t -> do
    mPricing <- runClientOrErr clientGetPricing
    AccountInfo {..} <- runClientOrErr $ clientGetAccountInfo t
    tickles <- runClientOrErr $ clientGetItemUUIDs t
    let canAdd =
          case mPricing of
            Nothing -> True
            Just Pricing {..} ->
              length tickles > pricingMaxItemsFree && isNothing accountInfoSubscribed
    token <- genToken
    withNavBar $(widgetFile "add")

data NewItem =
  NewItem
    { newItemText :: Textarea
    , newItemScheduledDay :: Day
    , newItemScheduledTime :: Maybe TimeOfDay
    , newItemRecurrenceData :: RecurrenceData
    }

data RecurrenceData =
  RecurrenceData
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
     mkOptionList $ map (\v -> Option (T.pack $ show v) v (T.pack $ show v)) [minBound .. maxBound])
    "recurrence" <*>
  iopt (checkMMap (pure . (pure :: a -> Either Text a) . fromInteger) fromIntegral intField) "days" <*>
  iopt timeField "day-time-of-day" <*>
  iopt
    (checkMMap (pure . (pure :: a -> Either Text a) . fromInteger) fromIntegral intField)
    "months" <*>
  iopt (checkMMap (pure . (pure :: a -> Either Text a) . fromInteger) fromIntegral intField) "day" <*>
  iopt timeField "month-time-of-day"

data RecurrenceOption
  = NoRecurrence
  | Days
  | Months
  deriving (Show, Read, Eq, Enum, Bounded)

-- The first maybe is for failure, the second is for whether or not there is recurrence.
mkRecurrence :: RecurrenceData -> Maybe (Maybe Recurrence)
mkRecurrence RecurrenceData {..} =
  case recurrenceDataOption of
    NoRecurrence -> pure Nothing
    Days ->
      case everyDaysAtTime (fromMaybe 1 recurrenceDataDays) recurrenceDataDayTimeOfDay of
        Nothing -> Nothing
        Just r -> Just $ Just r
    Months ->
      case everyMonthsOnDayAtTime
             (fromMaybe 1 recurrenceDataMonths)
             recurrenceDataMonthDay
             recurrenceDataMonthTimeOfDay of
        Nothing -> Nothing
        Just r -> Just $ Just r

postAddR :: Handler Html
postAddR =
  withLogin $ \t -> do
    AccountSettings {..} <- runClientOrErr $ clientGetAccountSettings t
    NewItem {..} <- runInputPost newItemForm
    case mkRecurrence newItemRecurrenceData of
      Nothing -> invalidArgs ["Invalid recurrence"]
      Just recurrence -> do
        errOrRes <-
          runClient $
          clientPostAddItem t $
          Tickle
            { tickleContent = textTypedItem $ unTextarea newItemText
            , tickleScheduledDay = newItemScheduledDay
            , tickleScheduledTime = newItemScheduledTime
            , tickleRecurrence = recurrence
            }
        case errOrRes of
          Left err ->
            handleStandardServantErrs err $ \resp ->
              case responseStatusCode resp of
                c
                  | c == Http.unauthorized401 ->
                    addNegativeMessage "You are not allowed to add items."
                  | c == Http.paymentRequired402 ->
                    addNegativeMessage
                      "You have reached the limit of the free plan, subscribe to be able to add more items. Click 'Account' to get started."
                  | otherwise -> sendResponseStatus Http.status500 $ show resp
          Right _ -> pure ()
    redirect AddR
