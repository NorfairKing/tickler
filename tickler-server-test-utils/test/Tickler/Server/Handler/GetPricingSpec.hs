{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module Tickler.Server.Handler.GetPricingSpec
  ( spec
  ) where

import TestImport

import Tickler.Client

import Tickler.API.Gen ()
import Tickler.Server.TestUtils

spec :: Spec
spec = do
  describe "Free" $
    withTicklerServerFree $
    describe "GetPricing" $
    it "gets an empty pricing" $ \cenv -> do
      p <- runClientOrError cenv clientGetPricing
      p `shouldBe` Nothing
  describe "Paid" $
    withTicklerServerPaid_ $
    describe "GetPricing" $
    it "gets a pricing" $ \cenv -> do
      p <- runClientOrError cenv clientGetPricing
      p `shouldSatisfy` isJust
