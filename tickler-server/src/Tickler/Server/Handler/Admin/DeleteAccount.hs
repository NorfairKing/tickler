{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeOperators #-}

module Tickler.Server.Handler.Admin.DeleteAccount
  ( serveAdminDeleteAccount,
  )
where

import Import
import Servant
import Tickler.API
import Tickler.Server.Handler.Utils
import Tickler.Server.Types

serveAdminDeleteAccount :: AuthCookie -> AccountUUID -> TicklerHandler NoContent
serveAdminDeleteAccount AuthCookie {..} uuid =
  withAdminCreds authCookieUserUUID $ do
    deleteAccountFully uuid
    pure NoContent
