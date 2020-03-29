{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DataKinds #-}

module Tickler.Server.Handler.Admin.GetAccounts
  ( serveAdminGetAccounts
  ) where

import Import

import Database.Persist

import Servant hiding (BadPassword, NoSuchUser)
import Servant.Auth.Server as Auth

import Tickler.API

import Tickler.Server.Types

import Tickler.Server.Handler.GetAccountInfo
import Tickler.Server.Handler.Utils

serveAdminGetAccounts :: AuthResult AuthCookie -> TicklerHandler [AccountInfo]
serveAdminGetAccounts (Authenticated AuthCookie {..}) =
  withAdminCreds authCookieUserUUID $ do
    users <- runDb $ selectList [] [Desc UserLastLogin]
    forM users $ \(Entity _ u) -> getUserAccountInfo u
serveAdminGetAccounts _ = throwAll err401
