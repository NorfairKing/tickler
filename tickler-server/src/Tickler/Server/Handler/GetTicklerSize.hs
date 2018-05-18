{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DataKinds #-}

module Tickler.Server.Handler.GetTicklerSize
    ( serveGetTicklerSize
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

serveGetTicklerSize :: AuthResult AuthCookie -> TicklerHandler Int
serveGetTicklerSize (Authenticated AuthCookie {..}) =
    runDb $ count [TicklerItemUserId ==. authCookieUserUUID]
serveGetTicklerSize _ = throwAll err401
