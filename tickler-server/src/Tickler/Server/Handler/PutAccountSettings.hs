{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DataKinds #-}

module Tickler.Server.Handler.PutAccountSettings
  ( servePutAccountSettings
  ) where

import Import

import Database.Persist

import Servant

import Tickler.API

import Tickler.Server.Types

import Tickler.Server.Handler.Utils

servePutAccountSettings :: AuthCookie -> AccountSettings -> TicklerHandler NoContent
servePutAccountSettings AuthCookie {..} AccountSettings {..} = do
  void $
    runDb $
    upsert
      UserSettings
        {userSettingsUserId = authCookieUserUUID, userSettingsTimeZone = accountSettingsTimeZone}
      [UserSettingsUserId =. authCookieUserUUID, UserSettingsTimeZone =. accountSettingsTimeZone]
  pure NoContent
