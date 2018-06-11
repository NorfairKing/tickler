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
    it "Going through the usual manual steps 'just works'" $ \(ClientEnv _ burl _) -> do
        tickler
            [ "register"
            , "--username"
            , "testuser"
            , "--password"
            , "testpass"
            , "--url"
            , showBaseUrl burl
            , "--tickler-dir"
            , "/tmp"
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
            , "/tmp"
            ]
        tickler ["add", "test", "2200-06-16", "--time", "15:23", "--tickler-dir", "/tmp"]
        tickler ["sync", "--url", showBaseUrl burl, "--tickler-dir", "/tmp"]
        tickler ["logout", "--tickler-dir", "/tmp"]
