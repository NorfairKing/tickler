{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Tickler.Server.Handler.AdminGetStatsSpec
  ( spec,
  )
where

import TestImport
import Tickler.API.Gen ()
import Tickler.Client
import Tickler.Server.TestUtils

spec :: Spec
spec =
  withTicklerServer
    $ describe "AdminGetStats"
    $ do
      it "forbids non-admin users from fetching admin stats" $ \cenv ->
        requiresAdmin cenv clientAdminGetStats
      it "returns valid admin stats" $ \cenv ->
        withAdmin cenv $ \token -> do
          adminStats <- runClientOrError cenv $ clientAdminGetStats token
          shouldBeValid adminStats
