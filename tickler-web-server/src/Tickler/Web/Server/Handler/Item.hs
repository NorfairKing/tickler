{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Tickler.Web.Server.Handler.Item where

import qualified Data.Text as T
import Data.Time
import Data.Word
import Import
import Text.Julius
import Tickler.Client
import Tickler.Web.Server.Foundation
import Yesod

handleEditItemForm :: Handler Tickle
handleEditItemForm = do
  EditItem {..} <- runInputPost editItemForm
  case mkRecurrence editItemRecurrenceData of
    Nothing -> invalidArgs ["Invalid recurrence"]
    Just recurrence -> do
      now <- liftIO getCurrentTime
      let (d, mtod) =
            case editItemScheduledDay of
              Nothing -> defaultScheduledDay now editItemScheduledTime recurrence
              Just r -> (r, editItemScheduledTime)
      pure
        Tickle
          { tickleContent = unTextarea editItemText,
            tickleScheduledDay = d,
            tickleScheduledTime = mtod,
            tickleRecurrence = recurrence
          }

data EditItem = EditItem
  { editItemText :: Textarea,
    editItemScheduledDay :: !(Maybe Day),
    editItemScheduledTime :: !(Maybe TimeOfDay),
    editItemRecurrenceData :: RecurrenceData
  }

data RecurrenceData = RecurrenceData
  { recurrenceDataOption :: RecurrenceOption,
    recurrenceDataDays :: !(Maybe Word),
    recurrenceDataDayTimeOfDay :: !(Maybe TimeOfDay),
    recurrenceDataMonths :: !(Maybe Word),
    recurrenceDataMonthDay :: !(Maybe Word8),
    recurrenceDataMonthTimeOfDay :: !(Maybe TimeOfDay)
  }

editItemForm :: FormInput Handler EditItem
editItemForm =
  EditItem <$> ireq textareaField "contents" <*> iopt dayField "scheduled-day"
    <*> iopt timeField "scheduled-time"
    <*> recurrenceDataForm

recurrenceDataForm :: FormInput Handler RecurrenceData
recurrenceDataForm =
  RecurrenceData
    <$> ireq
      ( radioField $
          pure $
            mkOptionList $
              map (\v -> Option (T.pack $ show v) v (T.pack $ show v)) [minBound .. maxBound]
      )
      "recurrence"
    <*> iopt (checkMMap (pure . (pure :: a -> Either Text a) . fromInteger) fromIntegral intField) "days"
    <*> iopt timeField "day-time-of-day"
    <*> iopt
      (checkMMap (pure . (pure :: a -> Either Text a) . fromInteger) fromIntegral intField)
      "months"
    <*> iopt (checkMMap (pure . (pure :: a -> Either Text a) . fromInteger) fromIntegral intField) "day"
    <*> iopt timeField "month-time-of-day"

data RecurrenceOption
  = NoRecurrence
  | Days
  | Months
  deriving (Show, Read, Eq, Enum, Bounded)

defaultScheduledDay :: UTCTime -> Maybe TimeOfDay -> Maybe Recurrence -> (Day, Maybe TimeOfDay)
defaultScheduledDay now mtod mrec =
  case mrec of
    Nothing -> (utctDay $ addUTCTime nominalDay now, mtod)
    Just r -> nextScheduledTime (utctDay now) mtod r

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

makeEditItemFormWidget :: Maybe (ItemUUID, ItemInfo) -> Handler Widget
makeEditItemFormWidget mii = do
  token <- genToken
  let mUUID = fst <$> mii
      mItem = itemInfoContents . snd <$> mii
      mContents = tickleContent <$> mItem
      mRec = mItem >>= tickleRecurrence
  -- This is quite the mess, but it works ..?
  let (noRecurrenceChecked, everyXDaysChecked, everyXMonthsChecked) =
        case mRec of
          Nothing -> (True, False, False)
          Just EveryDaysAtTime {} -> (False, True, False)
          Just EveryMonthsOnDay {} -> (False, False, True)
      selectedString =
        case mRec of
          Nothing -> "'None'" :: Text
          Just EveryDaysAtTime {} -> "'EveryDay'"
          Just EveryMonthsOnDay {} -> "'EveryMonth'"
      scheduledDayVal = maybe "" (show . tickleScheduledDay) mItem
      scheduledTimeVal = maybe "" show $ mItem >>= tickleScheduledTime
      (daysVal, timeVal) =
        case mRec of
          Nothing -> ("", "")
          Just (EveryDaysAtTime d tod) -> (show d, maybe "" show tod)
          Just EveryMonthsOnDay {} -> ("", "")
      (monthsVal, monthsDayVal, monthsTodVal) =
        case mRec of
          Nothing -> ("", "", "")
          Just EveryDaysAtTime {} -> ("", "", "")
          Just (EveryMonthsOnDay ms md mtod) -> (show ms, maybe "" show md, maybe "" show mtod)
  pure $(widgetFile "item")
