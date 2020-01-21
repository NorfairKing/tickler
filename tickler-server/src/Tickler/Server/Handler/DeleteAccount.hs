{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DataKinds #-}

module Tickler.Server.Handler.DeleteAccount
  ( serveDeleteAccount
  ) where

import Import

import Servant hiding (BadPassword, NoSuchUser)
import Servant.Auth.Server as Auth

import Tickler.API

import Tickler.Server.Types

import Tickler.Server.Handler.Utils

serveDeleteAccount :: AuthResult AuthCookie -> TicklerHandler NoContent
serveDeleteAccount (Authenticated AuthCookie {..}) = do
  deleteAccountFully authCookieUserUUID
  pure NoContent
serveDeleteAccount _ = throwAll err401
