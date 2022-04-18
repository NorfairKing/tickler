{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Tickler.Server.Handler.Protected.PutItem (servePutItem) where

import Database.Persist
import Import
import Servant
import Tickler.API
import Tickler.Server.Handler.Utils
import Tickler.Server.Types

servePutItem ::
  AuthCookie ->
  ItemUUID ->
  Tickle ->
  TicklerHandler NoContent
servePutItem AuthCookie {..} uuid Tickle {..} = do
  mI <- runDb $ getBy (UniqueItemIdentifier uuid)
  case mI of
    Nothing -> throwError err404
    Just (Entity i TicklerItem {..}) ->
      if ticklerItemUserId == authCookieUserUUID
        then do
          runDb $
            update
              i
              [ TicklerItemContents =. tickleContent,
                TicklerItemScheduledDay =. tickleScheduledDay,
                TicklerItemScheduledTime =. tickleScheduledTime,
                TicklerItemRecurrence =. tickleRecurrence
              ]
          pure NoContent
        else throwError err404
