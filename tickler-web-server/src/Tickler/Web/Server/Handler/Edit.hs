{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Tickler.Web.Server.Handler.Edit
  ( getEditR
  , postEditR
  ) where

import Import
import Tickler.Client
import Tickler.Web.Server.Foundation
import Yesod

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
    runClientOrErr $ do
      NoContent <- clientPostItem t uuid newVersion
      pure ()
    redirect $ EditR uuid
