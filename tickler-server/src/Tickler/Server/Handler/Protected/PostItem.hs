{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeOperators #-}

module Tickler.Server.Handler.Protected.PostItem
  ( servePostItem,
  )
where

import Database.Persist
import Import
import Servant
import Tickler.API
import Tickler.Server.Handler.Utils
import Tickler.Server.Types

servePostItem :: AuthCookie -> ItemUUID -> Tickle TypedItem -> TicklerHandler NoContent
servePostItem AuthCookie {..} uuid Tickle {..} = do
  mI <- runDb $ getBy (UniqueItemIdentifier uuid)
  case mI of
    Nothing -> throwError $ err404 {errBody = "Item not found."}
    Just (Entity i TicklerItem {..}) ->
      if ticklerItemUserId == authCookieUserUUID
        then do
          let TypedItem {..} = tickleContent
          runDb $
            update
              i
              [ TicklerItemType =. itemType,
                TicklerItemContents =. itemData,
                TicklerItemScheduledDay =. tickleScheduledDay,
                TicklerItemScheduledTime =. tickleScheduledTime,
                TicklerItemRecurrence =. tickleRecurrence
              ]
          pure NoContent
        else throwError $ err401 {errBody = "You are not allowed to edit this item."}
