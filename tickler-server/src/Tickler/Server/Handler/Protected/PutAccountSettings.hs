{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}

module Tickler.Server.Handler.Protected.PutAccountSettings
  ( servePutAccountSettings,
  )
where

import Database.Persist
import Import
import Servant
import Tickler.API
import Tickler.Server.Handler.Utils
import Tickler.Server.Types

servePutAccountSettings :: AuthCookie -> AccountSettings -> TicklerHandler NoContent
servePutAccountSettings AuthCookie {..} AccountSettings {..} = do
  void
    $ runDB
    $ upsert
      UserSettings
        { userSettingsUserId = authCookieUserUUID,
          userSettingsTimeZone = accountSettingsTimeZone
        }
      [UserSettingsUserId =. authCookieUserUUID, UserSettingsTimeZone =. accountSettingsTimeZone]
  pure NoContent
