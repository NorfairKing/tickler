{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}

module Tickler.Server.Stripe
  ( runStripeWith,
  )
where

import Data.Aeson
import Import
import Tickler.Server.OptParse.Types
import Web.Stripe as Stripe (StripeError, StripeRequest, StripeReturn, stripe)

runStripeWith ::
  FromJSON (StripeReturn a) =>
  StripeSettings ->
  StripeRequest a ->
  IO (Either StripeError (StripeReturn a))
runStripeWith StripeSettings {..} = stripe stripeSetStripeConfig
