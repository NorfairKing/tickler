{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

module Tickler.Server.Looper.StripeEventsFetcher where

import Import

import Control.Monad.Logger

import qualified Web.Stripe as Stripe
import Web.Stripe as Stripe ((-&-))
import qualified Web.Stripe.Customer as Stripe
import qualified Web.Stripe.Event as Stripe
import qualified Web.Stripe.Session as Stripe

import Conduit
import Web.Stripe.Conduit

import Database.Persist

import Tickler.Data
import Tickler.Server.Looper.DB
import Tickler.Server.Looper.Types
import Tickler.Server.OptParse.Types

runStripeEventsFetcher :: StripeSettings -> Looper ()
runStripeEventsFetcher ss = do
  let stripeConfig = stripeSetStripeConfig ss
  let fetchConduit =
        stripeConduit
          stripeConfig
          (Stripe.getEvents -&- Stripe.CheckoutSessionCompletedEvent)
          Stripe.eventId
  runConduit $ fetchConduit .| dealWithEventC

dealWithEventC :: ConduitT Stripe.Event Void Looper ()
dealWithEventC = do
  me <- await
  case me of
    Nothing -> pure ()
    Just e -> do
      lift $ dealWithEvent e
      dealWithEventC

dealWithEvent :: Stripe.Event -> Looper ()
dealWithEvent e = do
  mse <- runDb $ getBy $ UniqueStripeEvent $ Stripe.eventId e
  case mse of
    Just _ -> pure () -- No need to re-do this
    Nothing -> do
      se <- handleEvent e
      runDb $ insert_ se

handleEvent :: Stripe.Event -> Looper StripeEvent
handleEvent Stripe.Event {..} =
  let err t = do
        logErr t
        pure $ StripeEvent {stripeEventEvent = eventId, stripeEventError = Just t}
   in case eventType of
        Stripe.CheckoutSessionCompletedEvent ->
          case eventData of
            Stripe.CheckoutEvent Stripe.Session {..} ->
              case sessionData of
                Stripe.SessionSubscription eCus _ ->
                  case sessionClientReferenceId of
                    Just crid ->
                      case parseUUID (Stripe.getClientReferenceId crid) of
                        Just au ->
                          completePayment eventId au $
                          case eCus of
                            Stripe.Id cid -> cid
                            Stripe.Expanded c -> Stripe.customerId c
                        Nothing -> err "Client reference id didn't look like an AccountUUID"
                    Nothing -> err "No client reference id"
                _ -> err "Unknown session mode"
            _ -> err "Unknown event data"
        _ -> err "Unknown event"

completePayment :: Stripe.EventId -> AccountUUID -> Stripe.CustomerId -> Looper StripeEvent
completePayment eventId account cid = do
  void $
    runDb $
    upsertBy
      (UniqueCustomerUser account)
      (Customer {customerUser = account, customerStripeCustomer = cid})
      [CustomerStripeCustomer =. cid]
  pure StripeEvent {stripeEventEvent = eventId, stripeEventError = Nothing}

logErr :: Text -> Looper ()
logErr = logErrorNS "stripe-events-fetcher"
