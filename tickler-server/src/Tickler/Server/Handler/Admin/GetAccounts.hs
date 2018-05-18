{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DataKinds #-}

module Tickler.Server.Handler.Admin.GetAccounts
    ( serveAdminGetAccounts
    ) where

import Import

import Database.Persist

import Servant hiding (BadPassword, NoSuchUser)
import Servant.Auth.Server as Auth
import Servant.Auth.Server.SetCookieOrphan ()

import Tickler.API
import Tickler.Data

import Tickler.Server.Types

import Tickler.Server.Handler.Utils

serveAdminGetAccounts :: AuthResult AuthCookie -> TicklerHandler [AccountInfo]
serveAdminGetAccounts (Authenticated AuthCookie {..}) =
    withAdminCreds authCookieUserUUID $ do
        admins <- asks envAdmins
        users <- runDb $ selectList [] [Asc UserId]
        pure $
            flip map users $ \(Entity _ User {..}) ->
                AccountInfo
                { accountInfoUUID = userIdentifier
                , accountInfoUsername = userUsername
                , accountInfoCreatedTimestamp = userCreatedTimestamp
                , accountInfoLastLogin = userLastLogin
                , accountInfoAdmin = userUsername `elem` admins
                }
serveAdminGetAccounts _ = throwAll err401
