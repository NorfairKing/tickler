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

import Tickler.API

import Tickler.Server.Types

import Tickler.Server.Handler.Account
import Tickler.Server.Handler.Utils

serveAdminGetAccounts :: AuthCookie -> TicklerHandler [AccountInfo]
serveAdminGetAccounts AuthCookie {..} =
  withAdminCreds authCookieUserUUID $ do
    users <- runDb $ selectList [] [Desc UserLastLogin]
    forM users $ \(Entity _ u) -> getUserAccountInfo u
