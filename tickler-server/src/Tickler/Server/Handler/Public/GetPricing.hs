{-# LANGUAGE RecordWildCards #-}

module Tickler.Server.Handler.Public.GetPricing
  ( serveGetPricing,
  )
where

import Import
import Tickler.API
import Tickler.Server.OptParse.Types
import Tickler.Server.Types

serveGetPricing :: TicklerHandler (Maybe Pricing)
serveGetPricing = do
  mMone <- asks envMonetisation
  forM mMone $ \MonetisationSettings {..} -> do
    let StripeSettings {..} = monetisationSetStripeSettings
    let pricingPlan = stripeSetPlan
        pricingPrice = monetisationSetPrice
        pricingStripePublishableKey = stripeSetPublishableKey
        pricingMaxItemsFree = monetisationSetMaxItemsFree
    pure Pricing {..}
