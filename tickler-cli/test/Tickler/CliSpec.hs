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
  aroundWith (\adFunc a -> withSystemTempDir "tickler-cli-test" (\d -> adFunc (a, fromAbsDir d))) $
  it "Going through the usual manual steps 'just works'" $ \(ClientEnv _ burl _, tdir) -> do
    tickler
      [ "register"
      , "--username"
      , "testuser"
      , "--password"
      , "testpass"
      , "--url"
      , showBaseUrl burl
      , "--tickler-dir"
      , tdir
      ]
    tickler
      [ "login"
      , "--username"
      , "testuser"
      , "--password"
      , "testpass"
      , "--url"
      , showBaseUrl burl
      , "--tickler-dir"
      , tdir
      ]
    tickler ["add", "test1", "2200-06-16", "--time", "15:23", "--tickler-dir", tdir]
    tickler ["add", "test2", "2100-05-13", "--every-day", "--tickler-dir", tdir]
    tickler ["add", "test2", "2100-05-13", "--every-day", "--at", "12:08", "--tickler-dir", tdir]
    tickler ["add", "test3", "2100-05-12", "--every-x-days", "4", "--tickler-dir", tdir]
    tickler
      ["add", "test4", "2100-05-12", "--every-x-days", "3", "--at", "14:54", "--tickler-dir", tdir]
    tickler ["sync", "--url", showBaseUrl burl, "--tickler-dir", tdir]
    tickler ["add", "test5", "2100-04-13", "--every-month", "--tickler-dir", tdir]
    tickler
      ["add", "test6", "2100-04-12", "--every-x-months", "7", "--on", "12", "--tickler-dir", tdir]
    tickler
      [ "add"
      , "test7"
      , "2100-04-12"
      , "--every-x-months"
      , "8"
      , "--at"
      , "12:03"
      , "--tickler-dir"
      , tdir
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
      , "--tickler-dir"
      , tdir
      ]
    tickler ["sync", "--url", showBaseUrl burl, "--tickler-dir", tdir]
    tickler ["logout", "--tickler-dir", tdir]
