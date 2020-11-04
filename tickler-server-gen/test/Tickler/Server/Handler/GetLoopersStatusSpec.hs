{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Tickler.Server.Handler.GetLoopersStatusSpec
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
    $ describe "GetLoopersStatus"
    $ it "gets a valid loopers status"
    $ \cenv -> do
      r <- runClientOrError cenv clientGetLoopersInfo
      shouldBeValid r
