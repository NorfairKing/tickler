module TicklerServer where

import Import
import Tickler.Server (runTicklerServer)
import Tickler.Server.OptParse

ticklerServer :: IO ()
ticklerServer = getSettings >>= runTicklerServer
