module Tickler.Cli.TestUtils
  ( tickler
  ) where

import TestImport

import Tickler.Cli (ticklerCli)

tickler :: [String] -> IO ()
tickler args = do
  putStrLn $ unwords $ "RUNNING:" : "tickler" : args
  withArgs args ticklerCli
