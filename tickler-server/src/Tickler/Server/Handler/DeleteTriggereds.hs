{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DataKinds #-}

module Tickler.Server.Handler.DeleteTriggereds
    ( serveDeleteTriggereds
    ) where

import Import

import Database.Persist

import Servant hiding (BadPassword, NoSuchUser)
import Servant.Auth.Server as Auth
import Servant.Auth.Server.SetCookieOrphan ()

import Tickler.API
import Tickler.Data

import Tickler.Server.Types

import Tickler.Server.Handler.Utils

serveDeleteTriggereds :: AuthResult AuthCookie -> TicklerHandler NoContent
serveDeleteTriggereds (Authenticated AuthCookie {..}) = do
    runDb $ deleteWhere [TriggeredItemUserId ==. authCookieUserUUID]
    pure NoContent
serveDeleteTriggereds _ = throwAll err401
