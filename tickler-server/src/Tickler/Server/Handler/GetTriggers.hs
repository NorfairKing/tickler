{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DataKinds #-}

module Tickler.Server.Handler.GetTriggers where

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

serveGetTriggers ::
       AuthResult AuthCookie
    -> TicklerHandler [TriggerInfo]
serveGetTriggers (Authenticated AuthCookie {..})  = pure undefined
serveGetTriggers _ = throwAll err401
