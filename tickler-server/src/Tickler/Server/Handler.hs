module Tickler.Server.Handler
  ( ticklerServer
  ) where

import Servant.Auth.Server
import Servant.Server
import Servant.Server.Generic

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
import Tickler.Server.Handler.PostEmailTriggerVerify (servePostEmailTriggerVerify)
import Tickler.Server.Handler.PostSync (servePostSync)
import Tickler.Server.Handler.Public (ticklerPublicServer)
import Tickler.Server.Handler.PutAccountSettings (servePutAccountSettings)
import Tickler.Server.Handler.RetryTriggered (serveRetryTriggered)

ticklerServer :: TicklerSite (AsServerT TicklerHandler)
ticklerServer =
  TicklerSite
    {openSite = genericServerT ticklerOpenServer, adminSite = genericServerT ticklerAdminServer}

ticklerOpenServer :: TicklerOpenSite (AsServerT TicklerHandler)
ticklerOpenServer =
  TicklerOpenSite
    { protectedSite = genericServerT ticklerProtectedServer
    , publicSite = genericServerT ticklerPublicServer
    }

ticklerProtectedServer :: TicklerProtectedSite (AsServerT TicklerHandler)
ticklerProtectedServer =
  TicklerProtectedSite
    { getItemUUIDs = withAuthResult serveGetItemUUIDs
    , getItems = withAuthResult serveGetItems
    , postAddItem = withAuthResult servePostAddItem
    , getItem = withAuthResult serveGetItem
    , deleteItem = withAuthResult serveDeleteItem
    , retryTriggered = withAuthResult serveRetryTriggered
    , deleteTriggereds = withAuthResult serveDeleteTriggereds
    , postSync = withAuthResult servePostSync
    , getTriggers = withAuthResult serveGetTriggers
    , getTrigger = withAuthResult serveGetTrigger
    , postAddIntrayTrigger = withAuthResult servePostAddIntrayTrigger
    , postAddEmailTrigger = withAuthResult servePostAddEmailTrigger
    , postEmailTriggerVerify = withAuthResult servePostEmailTriggerVerify
    , postEmailTriggerResendVerificationEmail =
        withAuthResult servePostEmailTriggerResendVerificationEmail
    , deleteTrigger = withAuthResult serveDeleteTrigger
    , getAccountInfo = withAuthResult serveGetAccountInfo
    , getAccountSettings = withAuthResult serveGetAccountSettings
    , putAccountSettings = withAuthResult servePutAccountSettings
    , deleteAccount = withAuthResult serveDeleteAccount
    }

withAuthResult :: ThrowAll a => (AuthCookie -> a) -> (AuthResult AuthCookie -> a)
withAuthResult func ar =
  case ar of
    Authenticated ac -> func ac
    _ -> throwAll err401
