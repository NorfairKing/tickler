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
import Servant.Auth.Server as Auth

import Tickler.API
import Tickler.Data

import Tickler.Server.Types

import Tickler.Server.Handler.Utils

servePutAccountSettings ::
       AuthResult AuthCookie -> AccountSettings -> TicklerHandler NoContent
servePutAccountSettings (Authenticated AuthCookie {..}) AccountSettings {..} = do
    void $
        runDb $
        upsert
            UserSettings
                { userSettingsUserId = authCookieUserUUID
                , userSettingsTimeZone = accountSettingsTimeZone
                }
            [ UserSettingsUserId =. authCookieUserUUID
            , UserSettingsTimeZone =. accountSettingsTimeZone
            ]
    pure NoContent
servePutAccountSettings _ _ = throwAll err401
