{-# LANGUAGE OverloadedStrings #-}

module Tickler.Cli.NoSyncSpec
  ( spec
  ) where

import TestImport

import Tickler.Cli
import Tickler.Cli.OptParse

spec :: Spec
spec -- do
    -- it "Works fine without a server" $ do
    --     let sets =
    --             Settings
    --                 { setBaseUrl = Nothing
    --                 , setUsername = Nothing
    --                 , setTicklerDir = $(mkAbsDir "/tmp")
    --                 , setSyncStrategy = NeverSync
    --                 }
    --     let tickler d = runReaderT (dispatch d) sets
    --     -- tickler $ DispatchPostPostAddItem "hello world"
    --     -- tickler DispatchShowItem
    --     -- tickler DispatchDoneItem
    --     -- tickler DispatchSize
    --     pure () :: IO ()
 =
  around (withSystemTempDir "tickler-cli-test") $
  specify "login fails immediately if no server is configured" $ \tdir -> do
    let sets =
          Settings
            { setBaseUrl = Nothing
            , setUsername = Nothing
            , setTicklerDir = tdir
            , setSyncStrategy = NeverSync
            }
    let tickler d = runReaderT (dispatch d) sets
    tickler (DispatchLogin LoginSettings {loginSetUsername = Nothing, loginSetPassword = Nothing}) `shouldThrow`
      (== ExitFailure 1)
