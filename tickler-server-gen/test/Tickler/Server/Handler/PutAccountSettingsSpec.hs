{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Tickler.Server.Handler.PutAccountSettingsSpec
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
    $ describe "PutAccountSettings"
    $ it "replace the previous account settings"
    $ \cenv ->
      forAllValid $ \sets ->
        withValidNewUser cenv $ \token -> do
          sets' <-
            runClientOrError cenv $ do
              NoContent <- clientPutAccountSettings token sets
              clientGetAccountSettings token
          sets' `shouldBe` sets
