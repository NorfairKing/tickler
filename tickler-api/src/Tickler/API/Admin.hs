{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Tickler.API.Admin
    ( TicklerAdminAPI
    , TicklerAdminSite(..)
    , AdminStats(..)
    , AdminGetStats
    , AdminDeleteAccount
    , AdminGetAccounts
    ) where

import Import

import Servant.API
import Servant.Auth.Docs ()
import Servant.Auth.Server.SetCookieOrphan ()
import Servant.Generic

import Tickler.Data

import Tickler.API.Admin.Types
import Tickler.API.Account.Types
import Tickler.API.Types

type TicklerAdminAPI = ToServant (TicklerAdminSite AsApi)

data TicklerAdminSite route = TicklerAdminSite
    { adminGetStats :: route :- AdminGetStats
    , adminDeleteAccount :: route :- AdminDeleteAccount
    , adminGetAccounts :: route :- AdminGetAccounts
    } deriving (Generic)

type AdminGetStats = ProtectAPI :> "stats" :> Get '[ JSON] AdminStats

type AdminDeleteAccount
     = ProtectAPI :> "account" :> Capture "id" AccountUUID :> Delete '[ JSON] NoContent

type AdminGetAccounts = ProtectAPI :> "accounts" :> Get '[ JSON] [AccountInfo]
