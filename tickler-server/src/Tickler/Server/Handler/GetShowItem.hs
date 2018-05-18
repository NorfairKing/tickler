{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DataKinds #-}

module Tickler.Server.Handler.GetShowItem where

import Import

import Database.Persist

import Servant hiding (BadPassword, NoSuchUser)
import Servant.Auth.Server as Auth
import Servant.Auth.Server.SetCookieOrphan ()

import Tickler.API
import Tickler.Data

import Tickler.Server.Types

import Tickler.Server.Handler.Utils
import Tickler.Server.Item

serveGetShowItem ::
       AuthResult AuthCookie -> TicklerHandler (Maybe (ItemInfo TypedItem))
serveGetShowItem (Authenticated AuthCookie {..}) = do
    itemsEnt <-
        runDb $
        selectFirst
            [TicklerItemUserId ==. authCookieUserUUID]
            [Asc TicklerItemCreated]
    pure $ makeItemInfo . entityVal <$> itemsEnt
serveGetShowItem _ = throwAll err401
