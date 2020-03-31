{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DataKinds #-}

module Tickler.Server.Handler.Protected.DeleteAccount
  ( serveDeleteAccount
  ) where

import Import

import Servant hiding (BadPassword, NoSuchUser)

import Tickler.API

import Tickler.Server.Types

import Tickler.Server.Handler.Utils

serveDeleteAccount :: AuthCookie -> TicklerHandler NoContent
serveDeleteAccount AuthCookie {..} = do
  deleteAccountFully authCookieUserUUID
  pure NoContent
