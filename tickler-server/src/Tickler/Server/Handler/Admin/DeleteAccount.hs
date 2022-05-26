{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}

module Tickler.Server.Handler.Admin.DeleteAccount
  ( serveAdminDeleteAccount,
  )
where

import Database.Persist
import Import
import Servant
import Tickler.API
import Tickler.Server.Handler.Utils
import Tickler.Server.Types

serveAdminDeleteAccount :: AuthCookie -> Username -> TicklerHandler NoContent
serveAdminDeleteAccount AuthCookie {..} username = withAdminCreds authCookieUserUUID $ do
  mUserEntity <- runDB $ getBy $ UniqueUsername username
  case mUserEntity of
    Nothing -> throwError err404
    Just (Entity _ User {..}) -> do
      deleteAccountFully userIdentifier
      pure NoContent
