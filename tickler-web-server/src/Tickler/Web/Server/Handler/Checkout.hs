{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Tickler.Web.Server.Handler.Checkout
  ( getCheckoutR,
    getCheckoutSuccessR,
    getCheckoutCanceledR,
  )
where

import Data.Time
import Import
import Text.Julius
import Text.Time.Pretty
import Tickler.Client
import Tickler.Web.Server.Foundation
import Yesod

getCheckoutR :: Handler Html
getCheckoutR = do
  withLogin $ \t -> do
    mPricing <- runClientOrErr clientGetPricing
    AccountInfo {..} <- runClientOrErr $ clientGetAccountInfo t
    renderUrl <- getUrlRender
    InitiatedCheckoutSession {..} <-
      runClientOrErr $
        clientPostInitiateStripeCheckoutSession
          t
          InitiateStripeCheckoutSession
            { initiateStripeCheckoutSessionSuccessUrl = renderUrl CheckoutSuccessR,
              initiateStripeCheckoutSessionCanceledUrl = renderUrl CheckoutCanceledR
            }
    let stripeForm Pricing {..} = $(widgetFile "stripe-form")
    now <- liftIO getCurrentTime
    withNavBar $(widgetFile "checkout")

getCheckoutSuccessR :: Handler Html
getCheckoutSuccessR = redirect AccountR

getCheckoutCanceledR :: Handler Html
getCheckoutCanceledR = redirect AccountR
