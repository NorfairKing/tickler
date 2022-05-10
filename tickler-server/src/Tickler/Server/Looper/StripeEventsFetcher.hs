{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Tickler.Server.Looper.StripeEventsFetcher where

import Conduit
import Control.Monad.Logger
import qualified Data.Text as T
import Database.Persist
import Import
import Tickler.Data
import Tickler.Server.Looper.DB
import Tickler.Server.Looper.Types
import Tickler.Server.OptParse.Types
import Web.Stripe as Stripe ((-&-))
import qualified Web.Stripe as Stripe
import Web.Stripe.Conduit
import qualified Web.Stripe.Customer as Stripe
import qualified Web.Stripe.Event as Stripe
import qualified Web.Stripe.Session as Stripe

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
      logInfoN $ T.pack $ unwords ["Dealing with stripe event:", show e]
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
        runDb $
          insert_ $
            AdminNotificationEmail
              { adminNotificationEmailContents =
                  T.unlines ["The following problem occurred during Stripe event handling: ", t],
                adminNotificationEmailEmail = Nothing
              }
        pure $ StripeEvent {stripeEventEvent = eventId, stripeEventError = Just t}
   in case eventType of
        Stripe.CheckoutSessionCompletedEvent ->
          case eventData of
            Stripe.CheckoutEvent Stripe.Session {..} ->
              case sessionData of
                Stripe.SessionSubscription eCus _ ->
                  case sessionClientReferenceId of
                    Just crid ->
                      case parseUUIDText (Stripe.getClientReferenceId crid) of
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
        (UniqueStripeCustomer account cid)
        (StripeCustomer {stripeCustomerUser = account, stripeCustomerCustomer = cid})
        [StripeCustomerCustomer =. cid]
  pure StripeEvent {stripeEventEvent = eventId, stripeEventError = Nothing}

logErr :: Text -> Looper ()
logErr = logErrorNS "stripe-events-fetcher"
