{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}

module Tickler.Web.Server.Handler.Edit
  ( getEditR
  , postEditR
  ) where

import Import

import Yesod

import Tickler.Client

import Tickler.Web.Server.Foundation

getEditR :: ItemUUID -> Handler Html
getEditR uuid =
  withLogin $ \t -> do
    token <- genToken
    ItemInfo {..} <- runClientOrErr $ clientGetItem t uuid
    withNavBar $(widgetFile "edit")

data EditItem =
  EditItem
    { editItemContents :: Textarea
    }
  deriving (Show, Eq, Generic)

editItemForm :: FormInput Handler EditItem
editItemForm = EditItem <$> ireq textareaField "contents"

postEditR :: ItemUUID -> Handler Html
postEditR uuid =
  withLogin $ \t -> do
    AccountSettings {..} <- runClientOrErr $ clientGetAccountSettings t
    ItemInfo {..} <- runClientOrErr $ clientGetItem t uuid
    EditItem {..} <- runInputPost editItemForm
    let newVersion = itemInfoContents {tickleContent = textTypedItem $ unTextarea editItemContents}
    newUuid <-
      runClientOrErr $ do
        u <- clientPostAddItem t newVersion
        void $ clientDeleteItem t uuid
        pure u
    redirect $ EditR newUuid
