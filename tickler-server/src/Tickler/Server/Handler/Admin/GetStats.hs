{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DataKinds #-}

module Tickler.Server.Handler.Admin.GetStats
    ( serveAdminGetStats
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

serveAdminGetStats :: AuthResult AuthCookie -> TicklerHandler AdminStats
serveAdminGetStats (Authenticated AuthCookie {..}) =
    withAdminCreds authCookieUserUUID $ do
        adminStatsNbUsers <- runDb $ count ([] :: [Filter User])
        adminStatsNbItems <- runDb $ count ([] :: [Filter TicklerItem])
        pure AdminStats {..}
serveAdminGetStats _ = throwAll err401
