module Tickler.CliSpec
  ( spec
  ) where

import TestImport

import Servant.Client

import Tickler.Cli.TestUtils
import Tickler.Server.TestUtils

spec :: Spec
spec =
  withTicklerServer $
  it "Going through the usual manual steps 'just works'" $ \(ClientEnv _ burl _) ->
    withSystemTempDir "tickler-cli-test-data" $ \dataDir ->
      withSystemTempDir "tickler-cli-test-cache" $ \cacheDir -> do
        tickler
          [ "register"
          , "--username"
          , "testuser"
          , "--password"
          , "testpass"
          , "--url"
          , showBaseUrl burl
          , "--cache-dir"
          , fromAbsDir cacheDir
          , "--data-dir"
          , fromAbsDir dataDir
          ]
        tickler
          [ "login"
          , "--username"
          , "testuser"
          , "--password"
          , "testpass"
          , "--url"
          , showBaseUrl burl
          , "--cache-dir"
          , fromAbsDir cacheDir
          , "--data-dir"
          , fromAbsDir dataDir
          ]
        tickler
          [ "add"
          , "test1"
          , "2200-06-16"
          , "--time"
          , "15:23"
          , "--cache-dir"
          , fromAbsDir cacheDir
          , "--data-dir"
          , fromAbsDir dataDir
          ]
        tickler
          [ "add"
          , "test2"
          , "2100-05-13"
          , "--every-day"
          , "--cache-dir"
          , fromAbsDir cacheDir
          , "--data-dir"
          , fromAbsDir dataDir
          ]
        tickler
          [ "add"
          , "test2"
          , "2100-05-13"
          , "--every-day"
          , "--at"
          , "12:08"
          , "--cache-dir"
          , fromAbsDir cacheDir
          , "--data-dir"
          , fromAbsDir dataDir
          ]
        tickler
          [ "add"
          , "test3"
          , "2100-05-12"
          , "--every-x-days"
          , "4"
          , "--cache-dir"
          , fromAbsDir cacheDir
          , "--data-dir"
          , fromAbsDir dataDir
          ]
        tickler
          [ "add"
          , "test4"
          , "2100-05-12"
          , "--every-x-days"
          , "3"
          , "--at"
          , "14:54"
          , "--cache-dir"
          , fromAbsDir cacheDir
          , "--data-dir"
          , fromAbsDir dataDir
          ]
        tickler
          [ "sync"
          , "--url"
          , showBaseUrl burl
          , "--cache-dir"
          , fromAbsDir cacheDir
          , "--data-dir"
          , fromAbsDir dataDir
          ]
        tickler
          [ "add"
          , "test5"
          , "2100-04-13"
          , "--every-month"
          , "--cache-dir"
          , fromAbsDir cacheDir
          , "--data-dir"
          , fromAbsDir dataDir
          ]
        tickler
          [ "add"
          , "test6"
          , "2100-04-12"
          , "--every-x-months"
          , "7"
          , "--on"
          , "12"
          , "--cache-dir"
          , fromAbsDir cacheDir
          , "--data-dir"
          , fromAbsDir dataDir
          ]
        tickler
          [ "add"
          , "test7"
          , "2100-04-12"
          , "--every-x-months"
          , "8"
          , "--at"
          , "12:03"
          , "--cache-dir"
          , fromAbsDir cacheDir
          , "--data-dir"
          , fromAbsDir dataDir
          ]
        tickler
          [ "add"
          , "test8"
          , "2100-04-12"
          , "--every-x-months"
          , "9"
          , "--on"
          , "12"
          , "--at"
          , "12:03"
          , "--cache-dir"
          , fromAbsDir cacheDir
          , "--data-dir"
          , fromAbsDir dataDir
          ]
        tickler
          [ "sync"
          , "--url"
          , showBaseUrl burl
          , "--cache-dir"
          , fromAbsDir cacheDir
          , "--data-dir"
          , fromAbsDir dataDir
          ]
        tickler ["logout", "--cache-dir", fromAbsDir cacheDir, "--data-dir", fromAbsDir dataDir]
