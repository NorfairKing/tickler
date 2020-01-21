{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DataKinds #-}

module Tickler.Server.Handler.Admin.DeleteAccount
  ( serveAdminDeleteAccount
  ) where

import Import

import Servant hiding (BadPassword, NoSuchUser)
import Servant.Auth.Server as Auth

import Tickler.API

import Tickler.Server.Types

import Tickler.Server.Handler.Utils

serveAdminDeleteAccount :: AuthResult AuthCookie -> AccountUUID -> TicklerHandler NoContent
serveAdminDeleteAccount (Authenticated AuthCookie {..}) uuid =
  withAdminCreds authCookieUserUUID $ do
    deleteAccountFully uuid
    pure NoContent
serveAdminDeleteAccount _ _ = throwAll err401
