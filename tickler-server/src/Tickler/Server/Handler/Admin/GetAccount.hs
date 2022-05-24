{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Tickler.Server.Handler.Admin.GetAccount
  ( serveAdminGetAccount,
  )
where

import Database.Persist
import Import
import Servant
import Tickler.API
import Tickler.Server.AccountInfo
import Tickler.Server.Handler.Utils
import Tickler.Server.Types

serveAdminGetAccount :: AuthCookie -> Username -> TicklerHandler AccountInfo
serveAdminGetAccount AuthCookie {..} username = withAdminCreds authCookieUserUUID $ do
  mUserEntity <- runDb $ getBy $ UniqueUsername username
  case mUserEntity of
    Nothing -> throwError err404
    Just (Entity _ user) -> getAccountInfoForUser user
