{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}

module Tickler.Web.Server.Handler.Delete
    ( postDeleteTickleR
    , postDeleteTriggeredR
    ) where

import Import

import Yesod

import Tickler.API
import Tickler.Client

import Tickler.Web.Server.Foundation

newtype DeleteItem = DeleteItem
    { deleteItemUUID :: ItemUUID
    }

deleteItemForm :: FormInput Handler DeleteItem
deleteItemForm = DeleteItem <$> ireq hiddenField "item"

postDeleteTriggeredR :: Handler Html
postDeleteTriggeredR =
    withLogin $ \t -> do
        DeleteItem {..} <- runInputPost deleteItemForm
        void $ runClientOrErr $ clientDeleteItem t deleteItemUUID
        redirect TriggeredR

postDeleteTickleR :: Handler Html
postDeleteTickleR =
    withLogin $ \t -> do
        DeleteItem {..} <- runInputPost deleteItemForm
        void $ runClientOrErr $ clientDeleteItem t deleteItemUUID
        redirect TicklesR
