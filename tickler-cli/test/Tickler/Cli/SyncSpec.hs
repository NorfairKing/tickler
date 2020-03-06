{-# LANGUAGE OverloadedStrings #-}

module Tickler.Cli.SyncSpec
  ( spec
  ) where

import TestImport

import qualified Data.Text as T

import Data.Mergeful (clientStoreSize)

import Servant.API
import Servant.Client

import Tickler.Client
import Tickler.Server.TestUtils

import Tickler.Cli.OptParse
import Tickler.Cli.Session (loadToken)
import Tickler.Cli.Store (readStoreOrEmpty)
import Tickler.Cli.TestUtils

spec :: Spec
spec =
  withTicklerServer $
  it "correctly deletes the local LastSeen after a sync if the item has dissappeared remotely" $ \cenv ->
    forAllValid $ \ti ->
      withSystemTempDir "tickler-cli-test-cache" $ \cacheDir ->
        withSystemTempDir "tickler-cli-test-data" $ \dataDir ->
          withValidNewUserAndData cenv $ \un pw _ -> do
            let (ClientEnv _ burl _) = cenv
            let u = T.unpack $ usernameText un
            let p = T.unpack pw
            setEnv "TICKLER_USERNAME" u
            setEnv "TICKLER_PASSWORD" p
            setEnv "TICKLER_URL" $ showBaseUrl burl
            setEnv "TICKLER_CACHE_DIR" $ fromAbsDir cacheDir
            setEnv "TICKLER_DATA_DIR" $ fromAbsDir dataDir
            tickler ["login"]
            let sets =
                  Settings
                    { setBaseUrl = Just burl
                    , setUsername = Just un
                    , setCacheDir = cacheDir
                    , setDataDir = dataDir
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
            tickler ["sync"]
            s1 <- runReaderT readStoreOrEmpty sets
            NoContent <- runClientOrError cenv $ clientDeleteItem token uuid
            tickler ["sync"]
            s2 <- runReaderT readStoreOrEmpty sets
            clientStoreSize (storeTickles s1) `shouldBe` clientStoreSize (storeTickles s2) + 1
