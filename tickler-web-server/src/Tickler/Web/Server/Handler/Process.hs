{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}

module Tickler.Web.Server.Handler.Process
    ( getProcessR
    , postAddR
    , postDoneR
    ) where

import Import

import Data.Time

import Yesod

import Tickler.API
import Tickler.Client

import Tickler.Web.Server.Foundation
import Tickler.Web.Server.Time

getProcessR :: Handler Html
getProcessR =
    withLogin $ \t -> do
        mItemWidget <-
            do mItem <- runClientOrErr $ clientGetShowItem t
               case mItem of
                   Nothing -> pure Nothing
                   Just i -> Just <$> makeItemInfoWidget i
        nrItems <- runClientOrErr $ length <$> clientGetItemUUIDs t
        withNavBar $(widgetFile "process")

makeItemInfoWidget :: ItemInfo TypedItem -> Handler Widget
makeItemInfoWidget ItemInfo {..} = do
    token <- genToken
    createdWidget <- makeTimestampWidget itemInfoCreated
    scheduledWidget <- makeTimestampWidget itemInfoScheduled
    pure $(widgetFile "item")

data NewItem = NewItem
    { newItemText :: Textarea
    , newItemScheduledDay :: Day
    }

newItemForm :: FormInput Handler NewItem
newItemForm =
    NewItem <$> ireq textareaField "contents" <*> ireq dayField "scheduled"

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
                      LocalTime newItemScheduledDay midnight -- TODO this is probably very wrong, revisit later
                }
        redirect AddR

newtype DoneItem = DoneItem
    { doneItemUUID :: ItemUUID
    }

doneItemForm :: FormInput Handler DoneItem
doneItemForm = DoneItem <$> ireq hiddenField "item"

postDoneR :: Handler Html
postDoneR =
    withLogin $ \t -> do
        DoneItem {..} <- runInputPost doneItemForm
        void $ runClientOrErr $ clientDeleteItem t doneItemUUID
        redirect ProcessR
