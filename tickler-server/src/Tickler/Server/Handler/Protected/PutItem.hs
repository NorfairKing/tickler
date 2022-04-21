{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
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
  mI <- runDb $ getBy (UniqueItemIdentifier authCookieUserUUID uuid)
  case mI of
    Nothing -> throwError err404
    Just (Entity i TicklerItem {..}) -> do
      runDb $
        update
          i
          [ TicklerItemContents =. tickleContent,
            TicklerItemScheduledDay =. tickleScheduledDay,
            TicklerItemScheduledTime =. tickleScheduledTime,
            TicklerItemRecurrence =. tickleRecurrence
          ]
      pure NoContent
