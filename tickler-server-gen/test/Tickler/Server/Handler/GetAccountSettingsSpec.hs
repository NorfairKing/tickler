{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Tickler.Server.Handler.GetAccountSettingsSpec
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
    $ describe "GetAccountSettings"
    $ it "returns valid account settings"
    $ \cenv ->
      withValidNewUser cenv $ \token -> do
        accountInfo <- runClientOrError cenv $ clientGetAccountSettings token
        shouldBeValid accountInfo
