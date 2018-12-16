{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}

module Tickler.Web.Server.Handler.RetryTriggered
    ( postRetryTriggeredR
    , postRetryTriggeredsR
    ) where

import Import

import Yesod

import Tickler.API
import Tickler.Client

import Tickler.Web.Server.Foundation

newtype RetryItem = RetryItem
    { retryItemUUID :: ItemUUID
    }

retryItemForm :: FormInput Handler RetryItem
retryItemForm = RetryItem <$> ireq hiddenField "item"

postRetryTriggeredR :: Handler Html
postRetryTriggeredR =
    withLogin $ \t -> do
        RetryItem {..} <- runInputPost retryItemForm
        void $ runClientOrErr $ clientRetryTriggered t [retryItemUUID]
        redirect TriggeredsR

postRetryTriggeredsR :: Handler Html
postRetryTriggeredsR =
    withLogin $ \t -> do
        items <- runClientOrErr $ clientGetAllItems t
        void $
            runClientOrErr $
            clientRetryTriggered t $ map itemInfoIdentifier items
        redirect TriggeredsR
