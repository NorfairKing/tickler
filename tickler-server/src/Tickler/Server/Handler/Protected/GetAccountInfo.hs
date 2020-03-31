{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DataKinds #-}

module Tickler.Server.Handler.Protected.GetAccountInfo
  ( serveGetAccountInfo
  ) where

import Import

import Database.Persist

import Servant

import Tickler.API

import Tickler.Server.Types

import Tickler.Server.Handler.Account
import Tickler.Server.Handler.Utils

serveGetAccountInfo :: AuthCookie -> TicklerHandler AccountInfo
serveGetAccountInfo AuthCookie {..} = do
  mUser <- runDb $ getBy $ UniqueUserIdentifier authCookieUserUUID
  case mUser of
    Nothing -> throwError err404 {errBody = "User not found."}
    Just (Entity _ u) -> getUserAccountInfo u
