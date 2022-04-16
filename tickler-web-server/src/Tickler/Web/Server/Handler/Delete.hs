{-# LANGUAGE OverloadedStrings #-}

module Tickler.Web.Server.Handler.Delete
  ( postDeleteTickleR,
  )
where

import Import
import Tickler.API
import Tickler.Client
import Tickler.Web.Server.Foundation
import Yesod

deleteItemForm :: FormInput Handler ItemUUID
deleteItemForm = ireq hiddenField "item"

postDeleteTickleR :: Handler Html
postDeleteTickleR =
  withLogin $ \t -> do
    deleteItemUUID <- runInputPost deleteItemForm
    void $ runClientOrErr $ clientDeleteItem t deleteItemUUID
    redirect TicklesR
