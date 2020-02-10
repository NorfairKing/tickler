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

import Servant hiding (BadPassword, NoSuchUser)
import Servant.Auth.Server as Auth

import Tickler.API

import Tickler.Server.Types

import Tickler.Server.Handler.Utils

serveGetAccountInfo :: AuthResult AuthCookie -> TicklerHandler AccountInfo
serveGetAccountInfo (Authenticated AuthCookie {..}) = do
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
serveGetAccountInfo _ = throwAll err401
