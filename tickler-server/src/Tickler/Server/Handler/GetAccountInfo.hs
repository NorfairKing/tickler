{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DataKinds #-}

module Tickler.Server.Handler.GetAccountInfo
  ( serveGetAccountInfo
  ) where

import Import

import Database.Persist

import Servant

import Tickler.API

import Tickler.Server.Types

import Tickler.Server.Handler.Utils

serveGetAccountInfo :: AuthCookie -> TicklerHandler AccountInfo
serveGetAccountInfo AuthCookie {..} = do
  admins <- asks envAdmins
  mUser <- runDb $ getBy $ UniqueUserIdentifier authCookieUserUUID
  case mUser of
    Nothing -> throwError err404 {errBody = "User not found."}
    Just (Entity _ User {..}) ->
      pure
        AccountInfo
          { accountInfoUUID = authCookieUserUUID
          , accountInfoUsername = userUsername
          , accountInfoCreated = userCreated
          , accountInfoLastLogin = userLastLogin
          , accountInfoAdmin = userUsername `elem` admins
          }
