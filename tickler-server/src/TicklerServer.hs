module TicklerServer where

import Import

import Tickler.Server (runTicklerServer)
import Tickler.Server.OptParse (Dispatch(..), Instructions(..), Settings(..), getInstructions)

ticklerServer :: IO ()
ticklerServer = do
  Instructions (DispatchServe serveSets) Settings <- getInstructions
  putStrLn $ unlines ["Running tickler-server with these settings:", ppShow serveSets]
  runTicklerServer serveSets
