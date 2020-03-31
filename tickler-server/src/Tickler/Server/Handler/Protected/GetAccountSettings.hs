{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DataKinds #-}

module Tickler.Server.Handler.Protected.GetAccountSettings
  ( serveGetAccountSettings
  ) where

import Import

import Data.Time
import Database.Persist

import Tickler.API

import Tickler.Server.Types

import Tickler.Server.Handler.Utils

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
