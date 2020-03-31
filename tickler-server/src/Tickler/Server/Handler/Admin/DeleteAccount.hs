{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DataKinds #-}

module Tickler.Server.Handler.Admin.DeleteAccount
  ( serveAdminDeleteAccount
  ) where

import Import

import Servant

import Tickler.API

import Tickler.Server.Types

import Tickler.Server.Handler.Utils

serveAdminDeleteAccount :: AuthCookie -> AccountUUID -> TicklerHandler NoContent
serveAdminDeleteAccount AuthCookie {..} uuid =
  withAdminCreds authCookieUserUUID $ do
    deleteAccountFully uuid
    pure NoContent
