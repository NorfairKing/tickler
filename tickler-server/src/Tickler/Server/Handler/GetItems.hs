{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DataKinds #-}

module Tickler.Server.Handler.GetItems where

import Import

import Database.Persist

import Servant hiding (BadPassword, NoSuchUser)
import Servant.Auth.Server as Auth
import Servant.Auth.Server.SetCookieOrphan ()

import Tickler.API
import Tickler.Data

import Tickler.Server.Item
import Tickler.Server.Types

import Tickler.Server.Handler.Utils

serveGetItems :: AuthResult AuthCookie -> TicklerHandler [ItemInfo TypedItem]
serveGetItems (Authenticated AuthCookie {..}) = do
    itemsEnts <-
        runDb $
        selectList
            [TicklerItemUserId ==. authCookieUserUUID]
            [Asc TicklerItemCreated]
    pure $ map (makeItemInfo . entityVal) itemsEnts
serveGetItems _ = throwAll err401
