{-# LANGUAGE RecordWildCards #-}

module Tickler.Server.Handler.Public.GetPricing
  ( serveGetPricing,
  )
where

import Data.Cache as Cache
import Import
import Tickler.API
import Tickler.Server.Handler.Stripe
import Tickler.Server.OptParse.Types
import Tickler.Server.Types
import Web.Stripe.Plan as Stripe

serveGetPricing :: TicklerHandler (Maybe Pricing)
serveGetPricing = do
  mMone <- asks envMonetisation
  forM mMone $ \MonetisationEnv {..} -> do
    let StripeSettings {..} = monetisationEnvStripeSettings
    mPlan <- liftIO $ Cache.lookup monetisationEnvPlanCache stripeSetPlan
    Stripe.Plan {..} <-
      case mPlan of
        Nothing -> do
          plan <- runStripeHandlerOrErrorWith monetisationEnvStripeSettings $ getPlan stripeSetPlan
          liftIO $ Cache.insert monetisationEnvPlanCache stripeSetPlan plan
          pure plan
        Just plan -> pure plan
    let pricingPlan = stripeSetPlan
        pricingTrialPeriod = planTrialPeriodDays
        pricingPrice = Stripe.Amount planAmount
        pricingCurrency = planCurrency
        pricingStripePublishableKey = stripeSetPublishableKey
        pricingMaxItemsFree = monetisationEnvMaxItemsFree
    pure Pricing {..}
