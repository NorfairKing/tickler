{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}

module Tickler.Server.Handler.Protected.DeleteAccount
  ( serveDeleteAccount,
  )
where

import Import
import Servant hiding (BadPassword, NoSuchUser)
import Tickler.API
import Tickler.Server.Handler.Utils
import Tickler.Server.Types

serveDeleteAccount :: AuthCookie -> TicklerHandler NoContent
serveDeleteAccount AuthCookie {..} = do
  deleteAccountFully authCookieUserUUID
  pure NoContent
