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

import Servant hiding (BadPassword, NoSuchUser)
import Servant.Auth.Server as Auth

import Tickler.API
import Tickler.Data

import Tickler.Server.Types

import Tickler.Server.Handler.Utils

serveGetItemUUIDs :: AuthResult AuthCookie -> TicklerHandler [ItemUUID]
serveGetItemUUIDs (Authenticated AuthCookie {..}) =
  fmap (fmap $ ticklerItemIdentifier . entityVal) $
  runDb $ selectList [TicklerItemUserId ==. authCookieUserUUID] [Asc TicklerItemCreated]
serveGetItemUUIDs _ = throwAll err401
