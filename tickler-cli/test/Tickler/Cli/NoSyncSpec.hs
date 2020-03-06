{-# LANGUAGE OverloadedStrings #-}

module Tickler.Cli.NoSyncSpec
  ( spec
  ) where

import TestImport

import Data.Time

import Tickler.Cli
import Tickler.Cli.OptParse

spec :: Spec
spec = do
  it "Works fine without a server" $
    withSystemTempDir "tickler-cli-test-cache" $ \cacheDir ->
      withSystemTempDir "tickler-cli-test-data" $ \dataDir -> do
        let sets =
              Settings
                { setBaseUrl = Nothing
                , setUsername = Nothing
                , setCacheDir = cacheDir
                , setDataDir = dataDir
                , setSyncStrategy = NeverSync
                }
        let tickler d = runReaderT (dispatch d) sets
        let as =
              AddSettings
                { addSetTickleContent = "Hello world"
                , addSetTickleDate = fromGregorian 2020 01 01
                , addSetTickleTime = Nothing
                , addSetTickleRecurrence = Nothing
                }
        tickler $ DispatchAdd as
  specify "login fails immediately if no server is configured" $
    withSystemTempDir "tickler-cli-test-cache" $ \cacheDir ->
      withSystemTempDir "tickler-cli-test-data" $ \dataDir -> do
        let sets =
              Settings
                { setBaseUrl = Nothing
                , setUsername = Nothing
                , setCacheDir = cacheDir
                , setDataDir = dataDir
                , setSyncStrategy = NeverSync
                }
        let tickler d = runReaderT (dispatch d) sets
        tickler
          (DispatchLogin LoginSettings {loginSetUsername = Nothing, loginSetPassword = Nothing}) `shouldThrow`
          (== ExitFailure 1)
