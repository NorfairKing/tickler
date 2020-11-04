{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Tickler.Server.Handler.GetPricingSpec
  ( spec,
  )
where

import TestImport
import Tickler.API.Gen ()
import Tickler.Client
import Tickler.Server.TestUtils

spec :: Spec
spec = do
  describe "Free"
    $ withFreeTicklerServer
    $ describe "GetPricing"
    $ it "gets an empty pricing"
    $ \cenv -> do
      p <- runClientOrError cenv clientGetPricing
      p `shouldBe` Nothing
  describe "Paid"
    $ withPaidTicklerServer_
    $ describe "GetPricing"
    $ it "gets a pricing"
    $ \cenv -> do
      p <- runClientOrError cenv clientGetPricing
      p `shouldSatisfy` isJust
