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
    timestampWidget <- makeTimestampWidget itemInfoTimestamp
    pure $(widgetFile "item")

newtype NewItem = NewItem
    { newItemText :: Textarea
    }

newItemForm :: FormInput Handler NewItem
newItemForm = NewItem <$> ireq textareaField "contents"

postAddR :: Handler Html
postAddR =
    withLogin $ \t -> do
        NewItem {..} <- runInputPost newItemForm
        void $
            runClientOrErr $
            clientPostAddItem t $ textTypedItem $ unTextarea newItemText
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
