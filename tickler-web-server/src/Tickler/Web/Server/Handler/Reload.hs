module Tickler.Web.Server.Handler.Reload (getReloadR) where

import Tickler.Web.Server.Foundation
import Yesod.AutoReload

getReloadR :: Handler ()
getReloadR = getAutoReloadR
