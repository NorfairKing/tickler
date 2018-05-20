{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}

module Tickler.Web.Server.Handler.Add
    ( getAddR
    , postAddR
    ) where

import Import

import Data.Time

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
    }

newItemForm :: FormInput Handler NewItem
newItemForm =
    NewItem <$> ireq textareaField "contents" <*> ireq dayField "scheduled-day" <*>
    iopt timeField "scheduled-time"

postAddR :: Handler Html
postAddR =
    withLogin $ \t -> do
        AccountSettings {..} <- runClientOrErr $ clientGetAccountSettings t
        NewItem {..} <- runInputPost newItemForm
        void $
            runClientOrErr $
            clientPostAddItem
                t
                AddItem
                { addItemTypedItem = textTypedItem $ unTextarea newItemText
                , addItemScheduled =
                      localTimeToUTC accountSettingsTimeZone $
                      LocalTime newItemScheduledDay $
                      fromMaybe midnight newItemScheduledTime
                }
        redirect AddR
