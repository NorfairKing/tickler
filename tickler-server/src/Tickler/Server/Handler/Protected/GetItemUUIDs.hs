{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}

module Tickler.Server.Handler.Protected.GetItemUUIDs
  ( serveGetItemUUIDs,
  )
where

import Database.Persist
import Import
import Tickler.API
import Tickler.Server.Handler.Utils
import Tickler.Server.Types

serveGetItemUUIDs :: AuthCookie -> TicklerHandler [ItemUUID]
serveGetItemUUIDs AuthCookie {..} =
  fmap (fmap $ ticklerItemIdentifier . entityVal) $
    runDb $
      selectList [TicklerItemUserId ==. authCookieUserUUID] [Asc TicklerItemCreated]
