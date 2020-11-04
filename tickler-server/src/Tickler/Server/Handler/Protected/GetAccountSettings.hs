{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeOperators #-}

module Tickler.Server.Handler.Protected.GetAccountSettings
  ( serveGetAccountSettings,
  )
where

import Data.Time
import Database.Persist
import Import
import Tickler.API
import Tickler.Server.Handler.Utils
import Tickler.Server.Types

serveGetAccountSettings :: AuthCookie -> TicklerHandler AccountSettings
serveGetAccountSettings AuthCookie {..} = do
  mSets <- runDb $ getBy $ UniqueUserSettings authCookieUserUUID
  pure $
    case mSets of
      Nothing -> defaultAccountSettings
      Just (Entity _ UserSettings {..}) ->
        AccountSettings {accountSettingsTimeZone = userSettingsTimeZone}

defaultAccountSettings :: AccountSettings
defaultAccountSettings = AccountSettings {accountSettingsTimeZone = utc}
