{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DataKinds #-}

module Tickler.Server.Handler.GetAccountSettings
  ( serveGetAccountSettings
  ) where

import Import

import Data.Time
import Database.Persist

import Servant
import Servant.Auth.Server as Auth

import Tickler.API
import Tickler.Data

import Tickler.Server.Types

import Tickler.Server.Handler.Utils

serveGetAccountSettings :: AuthResult AuthCookie -> TicklerHandler AccountSettings
serveGetAccountSettings (Authenticated AuthCookie {..}) = do
  mSets <- runDb $ getBy $ UniqueUserSettings authCookieUserUUID
  pure $
    case mSets of
      Nothing -> defaultAccountSettings
      Just (Entity _ UserSettings {..}) ->
        AccountSettings {accountSettingsTimeZone = userSettingsTimeZone}
serveGetAccountSettings _ = throwAll err401

defaultAccountSettings :: AccountSettings
defaultAccountSettings = AccountSettings {accountSettingsTimeZone = utc}
