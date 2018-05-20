module Tickler.Server.Handler
    ( ticklerServer
    ) where

import Servant.Generic

import Tickler.API

import Tickler.Server.Types

import Tickler.Server.Handler.Admin (ticklerAdminServer)
import Tickler.Server.Handler.DeleteAccount (serveDeleteAccount)
import Tickler.Server.Handler.DeleteItem (serveDeleteItem)
import Tickler.Server.Handler.GetAccountInfo (serveGetAccountInfo)
import Tickler.Server.Handler.GetAccountSettings
       (serveGetAccountSettings)
import Tickler.Server.Handler.GetItem (serveGetItem)
import Tickler.Server.Handler.GetItemUUIDs (serveGetItemUUIDs)
import Tickler.Server.Handler.GetItems (serveGetItems)
import Tickler.Server.Handler.PostAddItem (servePostAddItem)
import Tickler.Server.Handler.PostSync (servePostSync)
import Tickler.Server.Handler.Public (ticklerPublicServer)
import Tickler.Server.Handler.PutAccountSettings
       (servePutAccountSettings)

ticklerServer :: TicklerSite (AsServerT TicklerHandler)
ticklerServer =
    TicklerSite
    { openSite = toServant ticklerOpenServer
    , adminSite = toServant ticklerAdminServer
    }

ticklerOpenServer :: TicklerOpenSite (AsServerT TicklerHandler)
ticklerOpenServer =
    TicklerOpenSite
    { protectedSite = toServant ticklerProtectedServer
    , publicSite = toServant ticklerPublicServer
    }

ticklerProtectedServer :: TicklerProtectedSite (AsServerT TicklerHandler)
ticklerProtectedServer =
    TicklerProtectedSite
    { getItemUUIDs = serveGetItemUUIDs
    , getItems = serveGetItems
    , postAddItem = servePostAddItem
    , getItem = serveGetItem
    , deleteItem = serveDeleteItem
    , postSync = servePostSync
    , getAccountInfo = serveGetAccountInfo
    , getAccountSettings = serveGetAccountSettings
    , putAccountSettings = servePutAccountSettings
    , deleteAccount = serveDeleteAccount
    }
