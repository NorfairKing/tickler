module Tickler.Server.Handler
    ( ticklerServer
    ) where

import Servant.Generic

import Tickler.API

import Tickler.Server.Types

import Tickler.Server.Handler.Admin (ticklerAdminServer)
import Tickler.Server.Handler.DeleteAccount (serveDeleteAccount)
import Tickler.Server.Handler.DeleteItem (serveDeleteItem)
import Tickler.Server.Handler.DeleteTrigger (serveDeleteTrigger)
import Tickler.Server.Handler.DeleteTriggereds (serveDeleteTriggereds)
import Tickler.Server.Handler.GetAccountInfo (serveGetAccountInfo)
import Tickler.Server.Handler.GetAccountSettings (serveGetAccountSettings)
import Tickler.Server.Handler.GetItem (serveGetItem)
import Tickler.Server.Handler.GetItemUUIDs (serveGetItemUUIDs)
import Tickler.Server.Handler.GetItems (serveGetItems)
import Tickler.Server.Handler.GetTrigger (serveGetTrigger)
import Tickler.Server.Handler.GetTriggers (serveGetTriggers)
import Tickler.Server.Handler.PostAddEmailTrigger (servePostAddEmailTrigger)
import Tickler.Server.Handler.PostAddIntrayTrigger (servePostAddIntrayTrigger)
import Tickler.Server.Handler.PostAddItem (servePostAddItem)
import Tickler.Server.Handler.PostEmailTriggerResendVerificationEmail
    ( servePostEmailTriggerResendVerificationEmail
    )
import Tickler.Server.Handler.PostEmailTriggerVerify
    ( servePostEmailTriggerVerify
    )
import Tickler.Server.Handler.PostSync (servePostSync)
import Tickler.Server.Handler.Public (ticklerPublicServer)
import Tickler.Server.Handler.PutAccountSettings (servePutAccountSettings)
import Tickler.Server.Handler.RetryTriggered (serveRetryTriggered)

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
        , retryTriggered = serveRetryTriggered
        , deleteTriggereds = serveDeleteTriggereds
        , postSync = servePostSync
        , getTriggers = serveGetTriggers
        , getTrigger = serveGetTrigger
        , postAddIntrayTrigger = servePostAddIntrayTrigger
        , postAddEmailTrigger = servePostAddEmailTrigger
        , postEmailTriggerVerify = servePostEmailTriggerVerify
        , postEmailTriggerResendVerificationEmail =
              servePostEmailTriggerResendVerificationEmail
        , deleteTrigger = serveDeleteTrigger
        , getAccountInfo = serveGetAccountInfo
        , getAccountSettings = serveGetAccountSettings
        , putAccountSettings = servePutAccountSettings
        , deleteAccount = serveDeleteAccount
        }
