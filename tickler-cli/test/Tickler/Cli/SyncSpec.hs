{-# LANGUAGE OverloadedStrings #-}

module Tickler.Cli.SyncSpec
  ( spec
  ) where

import TestImport

import qualified Data.Text as T

import Servant.API
import Servant.Client

import Tickler.Client
import Tickler.Server.TestUtils

import Tickler.Cli.OptParse
import Tickler.Cli.Session (loadToken)
import Tickler.Cli.TestUtils

spec :: Spec
spec =
  withTicklerServer $
  aroundWith (\adFunc a -> withSystemTempDir "tickler-cli-test" (\d -> adFunc (a, fromAbsDir d))) $
  it "correctly deletes the local LastSeen after a sync if the item has dissappeared remotely" $ \(cenv, d) ->
    forAllValid $ \ti ->
      withValidNewUserAndData cenv $ \un pw _ -> do
        let (ClientEnv _ burl _) = cenv
        let u = T.unpack $ usernameText un
        let p = T.unpack pw
        dir <- resolveDir' d
        tickler
          ["login", "--username", u, "--password", p, "--url", showBaseUrl burl, "--tickler-dir", d]
        let sets =
              Settings
                { setBaseUrl = Just burl
                , setUsername = Just un
                , setTicklerDir = dir
                , setSyncStrategy = NeverSync
                }
        mToken <- runReaderT loadToken sets
        token <-
          case mToken of
            Nothing -> do
              expectationFailure "Should have a token after logging in"
              undefined
            Just t -> pure t
        uuid <- runClientOrError cenv $ clientPostAddItem token ti
        tickler ["sync", "--url", showBaseUrl burl, "--tickler-dir", d]
        NoContent <- runClientOrError cenv $ clientDeleteItem token uuid
        tickler ["sync", "--url", showBaseUrl burl, "--tickler-dir", d]
