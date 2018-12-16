{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DataKinds #-}

module Tickler.Server.Handler.PostAddItem
    ( servePostAddItem
    ) where

import Import

import Data.Time
import Data.UUID.Typed
import Database.Persist

import Servant hiding (BadPassword, NoSuchUser)
import Servant.Auth.Server as Auth
import Servant.Auth.Server.SetCookieOrphan ()

import Tickler.API

import Tickler.Server.Item
import Tickler.Server.Types

import Tickler.Server.Handler.Utils

servePostAddItem :: AuthResult AuthCookie -> AddItem -> TicklerHandler ItemUUID
servePostAddItem (Authenticated AuthCookie {..}) ti = do
    now <- liftIO getCurrentTime
    uuid <- liftIO nextRandomUUID
    runDb $ insert_ $ makeTicklerItem authCookieUserUUID uuid now now ti
    pure uuid
servePostAddItem _ _ = throwAll err401
