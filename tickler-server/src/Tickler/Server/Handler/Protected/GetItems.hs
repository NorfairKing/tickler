{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}

module Tickler.Server.Handler.Protected.GetItems where

import Database.Persist
import Import
import Tickler.API
import Tickler.Server.Handler.Utils
import Tickler.Server.Item
import Tickler.Server.Types

serveGetItems :: AuthCookie -> TicklerHandler [TypedItemInfo]
serveGetItems AuthCookie {..} = do
  itemsEnts <-
    runDb $
      selectList [TicklerItemUserId ==. authCookieUserUUID] [Asc TicklerItemCreated]
  pure $ map (makeTicklerItemInfo . entityVal) itemsEnts
