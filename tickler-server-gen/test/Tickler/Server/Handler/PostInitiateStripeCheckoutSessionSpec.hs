{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Tickler.Server.Handler.PostInitiateStripeCheckoutSessionSpec (spec) where

import Test.Syd.Aeson
import TestImport
import Tickler.Client
import Tickler.Server.Handler.PostInitiateStripeCheckoutSession

spec :: Spec
spec = do
  it "tries to make the same PostCustomersRequestBody as before"
    $ goldenJSONValueFile "test_resources/stripe/post-customer.json"
    $ do
      username <- parseUsername "username"
      pure $ mkPostCustomersRequestBodyForUser username
  it "tries to make the same PostCheckoutSessionsRequestBody as before"
    $ goldenJSONValueFile "test_resources/stripe/post-checkout-session.json"
    $ do
      let initiateStripeCheckoutSession =
            InitiateStripeCheckoutSession
              { initiateStripeCheckoutSessionSuccessUrl = "http://tickler.example.com/stripe/success",
                initiateStripeCheckoutSessionCanceledUrl = "http://tickler.example.com/stripe/cancel"
              }
      username <- parseUsername "username"
      let customerId = "customer-id"
      let planId = "plan-id"
      pure $ mkPostCheckoutSessionsRequestBodyForUser initiateStripeCheckoutSession username customerId planId
