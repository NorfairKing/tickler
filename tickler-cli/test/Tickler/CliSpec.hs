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
        tickler
            [ "add"
            , "hello"
            , "world"
            , "--url"
            , showBaseUrl burl
            , "--tickler-dir"
            , "/tmp"
            ]
        tickler ["show", "--url", showBaseUrl burl, "--tickler-dir", "/tmp"]
        tickler ["done", "--url", showBaseUrl burl, "--tickler-dir", "/tmp"]
        tickler ["size", "--url", showBaseUrl burl, "--tickler-dir", "/tmp"]
        tickler ["sync", "--url", showBaseUrl burl, "--tickler-dir", "/tmp"]
        tickler ["logout", "--tickler-dir", "/tmp"]
