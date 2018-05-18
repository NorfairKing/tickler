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
import Tickler.Server.Handler.GetItem (serveGetItem)
import Tickler.Server.Handler.GetItemUUIDs (serveGetItemUUIDs)
import Tickler.Server.Handler.GetItems (serveGetItems)
import Tickler.Server.Handler.GetShowItem (serveGetShowItem)
import Tickler.Server.Handler.GetTicklerSize (serveGetTicklerSize)
import Tickler.Server.Handler.PostAddItem (servePostAddItem)
import Tickler.Server.Handler.PostSync (servePostSync)
import Tickler.Server.Handler.Public (ticklerPublicServer)

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
    { getShowItem = serveGetShowItem
    , getTicklerSize = serveGetTicklerSize
    , getItemUUIDs = serveGetItemUUIDs
    , getItems = serveGetItems
    , postAddItem = servePostAddItem
    , getItem = serveGetItem
    , deleteItem = serveDeleteItem
    , postSync = servePostSync
    , getAccountInfo = serveGetAccountInfo
    , deleteAccount = serveDeleteAccount
    }
