module Tickler.CliSpec
  ( spec,
  )
where

import Servant.Client
import TestImport
import Tickler.Cli.TestUtils
import Tickler.Server.TestUtils

spec :: Spec
spec = sequential $ -- uses 'withArgs'
  withTicklerServer $
    it "Going through the usual manual steps 'just works'" $
      \(ClientEnv _ burl _) ->
        withSystemTempDir "tickler-cli-test-data" $ \dataDir ->
          withSystemTempDir "tickler-cli-test-cache" $ \cacheDir -> do
            setEnv "TICKLER_USERNAME" "testuser"
            setEnv "TICKLER_PASSWORD" "testpassword"
            setEnv "TICKLER_URL" $ showBaseUrl burl
            setEnv "TICKLER_CACHE_DIR" $ fromAbsDir cacheDir
            setEnv "TICKLER_DATA_DIR" $ fromAbsDir dataDir
            tickler ["register"]
            tickler ["login"]
            tickler ["add", "test1", "2200-06-16", "--time", "15:23"]
            tickler ["add", "test2", "2100-05-13", "--every-day"]
            tickler ["add", "test2", "2100-05-13", "--every-day", "--at", "12:08"]
            tickler ["add", "test3", "2100-05-12", "--every-x-days", "4"]
            tickler ["add", "test4", "2100-05-12", "--every-x-days", "3", "--at", "14:54"]
            tickler ["sync"]
            tickler ["add", "test5", "2100-04-13", "--every-month"]
            tickler ["add", "test6", "2100-04-12", "--every-x-months", "7", "--on", "12"]
            tickler ["add", "test7", "2100-04-12", "--every-x-months", "8", "--at", "12:03"]
            tickler
              ["add", "test8", "2100-04-12", "--every-x-months", "9", "--on", "12", "--at", "12:03"]
            tickler ["sync"]
            tickler ["logout"]
