{-# LANGUAGE RecordWildCards #-}

module Tickler.Web.Server.Handler.Edit
  ( getEditR,
    postEditR,
  )
where

import Import
import Tickler.Client
import Tickler.Web.Server.Foundation
import Tickler.Web.Server.Handler.Item
import Yesod

getEditR :: ItemUUID -> Handler Html
getEditR uuid =
  withLogin $ \t -> do
    ii <- runClientOrErr $ clientGetItem t uuid
    w <- makeEditItemFormWidget (Just (uuid, ii))
    withNavBar w

postEditR :: ItemUUID -> Handler Html
postEditR uuid =
  withLogin $ \t -> do
    AccountSettings {..} <- runClientOrErr $ clientGetAccountSettings t
    tickle <- handleEditItemForm
    runClientOrErr $ do
      NoContent <- clientPostItem t uuid tickle
      pure ()
    redirect $ EditR uuid
