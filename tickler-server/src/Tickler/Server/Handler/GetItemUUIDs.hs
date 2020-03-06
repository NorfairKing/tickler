{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DataKinds #-}

module Tickler.Server.Handler.GetItemUUIDs
  ( serveGetItemUUIDs
  ) where

import Import

import Database.Persist

import Tickler.API

import Tickler.Server.Types

import Tickler.Server.Handler.Utils

serveGetItemUUIDs :: AuthCookie -> TicklerHandler [ItemUUID]
serveGetItemUUIDs AuthCookie {..} =
  fmap (fmap $ ticklerItemIdentifier . entityVal) $
  runDb $ selectList [TicklerItemUserId ==. authCookieUserUUID] [Asc TicklerItemCreated]
