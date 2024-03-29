{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}

module Tickler.Server.Handler.Protected.GetItem
  ( serveGetItem,
  )
where

import Database.Persist
import Import
import Servant
import Tickler.API
import Tickler.Server.Handler.Utils
import Tickler.Server.Item
import Tickler.Server.Types

serveGetItem :: AuthCookie -> ItemUUID -> TicklerHandler ItemInfo
serveGetItem AuthCookie {..} id_ = do
  mIItem <- runDB $ getBy $ UniqueItemIdentifier authCookieUserUUID id_
  case mIItem of
    Nothing -> throwError err404
    Just item -> pure $ makeTicklerItemInfo $ entityVal item
