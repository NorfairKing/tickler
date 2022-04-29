{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}

module Tickler.Server.Handler.Protected.DeleteItem
  ( serveDeleteItem,
  )
where

import Database.Persist
import Import
import Servant
import Tickler.API
import Tickler.Server.Handler.Utils
import Tickler.Server.Types

serveDeleteItem :: AuthCookie -> ItemUUID -> TicklerHandler NoContent
serveDeleteItem AuthCookie {..} id_ = do
  mItem <- runDb $ getBy $ UniqueItemIdentifier authCookieUserUUID id_
  case mItem of
    Nothing -> throwError err404
    Just (Entity itemId _) -> do
      runDb $ delete itemId
      pure NoContent
