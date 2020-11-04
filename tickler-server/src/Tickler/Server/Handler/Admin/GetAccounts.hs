{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeOperators #-}

module Tickler.Server.Handler.Admin.GetAccounts
  ( serveAdminGetAccounts,
  )
where

import Database.Persist
import Import
import Tickler.API
import Tickler.Server.Handler.Account
import Tickler.Server.Handler.Utils
import Tickler.Server.Types

serveAdminGetAccounts :: AuthCookie -> TicklerHandler [AccountInfo]
serveAdminGetAccounts AuthCookie {..} =
  withAdminCreds authCookieUserUUID $ do
    users <- runDb $ selectList [] [Desc UserLastLogin]
    forM users $ \(Entity _ u) -> getUserAccountInfo u
