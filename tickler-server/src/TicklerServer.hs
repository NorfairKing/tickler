module TicklerServer where

import Import

import Tickler.Server (runTicklerServer)
import Tickler.Server.OptParse
       (Dispatch(..), Settings(..), getInstructions)

ticklerServer :: IO ()
ticklerServer = do
    (DispatchServe serveSets, Settings) <- getInstructions
    putStrLn $ ppShow serveSets
    runTicklerServer serveSets
