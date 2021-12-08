module TicklerServer where

import Import
import Tickler.Server (runTicklerServer)
import Tickler.Server.OptParse

ticklerServer :: IO ()
ticklerServer = do
  settings <- getSettings
  putStrLn $ unlines ["Running tickler-server with these settings:", ppShow settings]
  runTicklerServer settings
