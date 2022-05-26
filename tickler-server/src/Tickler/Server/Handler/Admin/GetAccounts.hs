{-# LANGUAGE RecordWildCards #-}

module Tickler.Server.Handler.Admin.GetAccounts
  ( serveAdminGetAccounts,
  )
where

import Database.Persist
import Import
import Tickler.API
import Tickler.Server.AccountInfo
import Tickler.Server.Handler.Utils
import Tickler.Server.Types

serveAdminGetAccounts :: AuthCookie -> TicklerHandler [AccountInfo]
serveAdminGetAccounts AuthCookie {..} = withAdminCreds authCookieUserUUID $ do
  users <- runDB $ selectList [] [Desc UserLastLogin]
  forM users $ \(Entity _ user) -> getAccountInfoForUser user
