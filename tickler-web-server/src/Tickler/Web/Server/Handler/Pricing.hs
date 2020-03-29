{-# LANGUAGE RecordWildCards #-}

module Tickler.Web.Server.Handler.Pricing
  ( pricingShowAmountPerMonth
  , showAmountForPricing
  ) where

import Import

import Web.Stripe as Stripe
import Web.Stripe.Types as Stripe

import Tickler.Client

pricingShowAmountPerMonth :: Pricing -> String
pricingShowAmountPerMonth Pricing {..} =
  showAmountForPricing pricingCurrency (quotPrice pricingPrice 12)

showAmountForPricing :: Currency -> Amount -> String
showAmountForPricing cur (Amount i) =
  let (q, r) = quotRem i 100
      hundred =
        if r == 0
          then unwords [show q, show cur]
          else showAmount cur i
   in case cur of
        EUR -> hundred
        USD -> hundred
        CHF -> hundred
        _ -> showAmount cur i

quotPrice :: Amount -> Int -> Amount
quotPrice (Amount i) d = Amount $ i `quot` d
