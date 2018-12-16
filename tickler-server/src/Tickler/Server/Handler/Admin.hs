module Tickler.Server.Handler.Admin
    ( TicklerAdminSite
    , ticklerAdminServer
    ) where

import Servant.Generic

import Tickler.API

import Tickler.Server.Types

import Tickler.Server.Handler.Admin.DeleteAccount
import Tickler.Server.Handler.Admin.GetAccounts
import Tickler.Server.Handler.Admin.GetStats

ticklerAdminServer :: TicklerAdminSite (AsServerT TicklerHandler)
ticklerAdminServer =
    TicklerAdminSite
        { adminGetStats = serveAdminGetStats
        , adminDeleteAccount = serveAdminDeleteAccount
        , adminGetAccounts = serveAdminGetAccounts
        }
