{-# LANGUAGE OverloadedStrings #-}

module Tickler.Cli.SyncSpec
  ( spec,
  )
where

import qualified Data.Map as M
import Data.Mergeful
import qualified Data.Text as T
import Servant.API
import Servant.Client
import TestImport
import Tickler.Cli.OptParse
import Tickler.Cli.Session (loadToken)
import Tickler.Cli.Store
import Tickler.Cli.TestUtils
import Tickler.Client
import Tickler.Server.TestUtils

spec :: Spec
spec = do
  withTicklerServer $
    it "correctly deletes the local LastSeen after a sync if the item has dissappeared remotely" $
      \cenv ->
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
                        { setBaseUrl = Just burl,
                          setUsername = Just un,
                          setCacheDir = cacheDir,
                          setDataDir = dataDir,
                          setSyncStrategy = NeverSync
                        }
                mToken <- runReaderT loadToken sets
                token <-
                  case mToken of
                    Nothing -> expectationFailure "Should have a token after logging in"
                    Just t -> pure t
                uuid <- runClientOrError cenv $ clientPostAddItem token ti
                tickler ["sync"]
                s1 <- runReaderT readStoreOrEmpty sets
                NoContent <- runClientOrError cenv $ clientDeleteItem token uuid
                tickler ["sync"]
                s2 <- runReaderT readStoreOrEmpty sets
                clientStoreSize (storeTickles s1) `shouldBe` clientStoreSize (storeTickles s2) + 1
  let maxFree = 2
  withPaidTicklerServer maxFree $
    it "Can add items past the maximum allowed number of free items locally but not remotely" $
      \cenv ->
        withValidNewUserAndData cenv $ \un pw _ ->
          withSystemTempDir "tickler-cli-test-data" $ \dataDir ->
            withSystemTempDir "tickler-cli-test-cache" $ \cacheDir -> do
              let (ClientEnv _ burl _) = cenv
              setEnv "TICKLER_USERNAME" $ T.unpack $ usernameText un
              setEnv "TICKLER_PASSWORD" $ T.unpack pw
              setEnv "TICKLER_URL" $ showBaseUrl burl
              setEnv "TICKLER_CACHE_DIR" $ fromAbsDir cacheDir
              setEnv "TICKLER_DATA_DIR" $ fromAbsDir dataDir
              setEnv "TICKLER_SYNC_STRATEGY" "AlwaysSync"
              tickler ["login"]
              let sets =
                    Settings
                      { setUsername = Nothing,
                        setBaseUrl = Just burl,
                        setCacheDir = cacheDir,
                        setDataDir = dataDir,
                        setSyncStrategy = AlwaysSync
                      }
              let size =
                    flip runReaderT sets $ do
                      cs <- storeTickles <$> readStoreOrEmpty
                      pure (M.size (clientStoreAddedItems cs), M.size (clientStoreSyncedItems cs))
              size `shouldReturn` (0, 0)
              tickler ["add", "one", "2020-03-30"]
              tickler ["sync"]
              size `shouldReturn` (0, 1)
              tickler ["add", "two", "2020-03-30"]
              tickler ["sync"]
              size `shouldReturn` (0, 2)
              tickler ["add", "three", "2020-03-30"]
              tickler ["sync"]
              size `shouldReturn` (1, 2)
