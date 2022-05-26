{-# LANGUAGE OverloadedStrings #-}

module Tickler.Server.Handler.PostStripeHook (servePostStripeHook) where

import Control.Monad.Logger
import Data.Aeson as JSON
import Data.Aeson.Encode.Pretty as JSON
import Data.Aeson.Types as JSON
import qualified Data.ByteString.Lazy as LB
import qualified Data.HashMap.Strict as HM
import Data.Maybe
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Time.Clock.POSIX
import Database.Persist
import Import as Tickler
import Servant
import StripeClient as Stripe
import Tickler.API as Tickler
import Tickler.Server.Handler.Utils
import Tickler.Server.Types

servePostStripeHook :: JSON.Value -> TicklerHandler NoContent
servePostStripeHook value = do
  logDebugNS "stripe-hook" $
    T.unlines
      [ "Got a request from Stripe:",
        TE.decodeUtf8 $ LB.toStrict $ JSON.encodePretty value
      ]
  case JSON.parseEither parseJSON value of
    Left err -> throwError $ err400 {errBody = LB.fromStrict $ TE.encodeUtf8 $ "Failed to parse event value from stripe: " <> T.pack err}
    Right event -> do
      let fullfillWith :: FromJSON a => (a -> TicklerHandler ()) -> TicklerHandler ()
          fullfillWith func = case parseEither parseJSON (toJSON (notificationEventDataObject (eventData event))) of
            Left err -> throwError $ err400 {errBody = LB.fromStrict $ TE.encodeUtf8 $ "Failed to parse event data in event with id " <> eventId event <> ": " <> T.pack err}
            Right r -> func r
      case eventType event of
        "customer.subscription.created" -> fullfillWith fullfillSubscription
        "customer.subscription.updated" -> fullfillWith fullfillSubscription
        "customer.subscription.deleted" -> fullfillWith fullfillSubscription
        t -> logInfoNS "stripe-hook" $ "Not handling event of type: " <> T.pack (show t)
  pure NoContent

-- | Update the subscription date when the subscription has been updated
fullfillSubscription :: Stripe.Subscription -> TicklerHandler ()
fullfillSubscription subscription = do
  -- We don't want to do anything with subscriptions for other products.
  case HM.lookup "product" $ subscriptionMetadata subscription of
    Nothing -> logInfoNS "stripe-hook" "Not fulfilling subscription without product."
    Just "tickler" -> do
      logInfoNS "stripe-hook" $ T.pack $ unlines ["fulfilling subscription:", ppShow subscription]
      customerId_ <- case subscriptionCustomer subscription of
        SubscriptionCustomer'Text cid -> pure cid
        SubscriptionCustomer'Customer c -> pure $ customerId c
        SubscriptionCustomer'DeletedCustomer _ -> throwError err400 {errBody = "Customer in subscription was a deleted customer."}

      -- Try to find the corresponding stripe customer so that we can figure out the user that this subscription belongs to
      mStripeCustomer <- runDb $ selectFirst [StripeCustomerCustomer ==. customerId_] [Desc StripeCustomerId]
      uid <- case mStripeCustomer of
        Nothing -> throwError err404 {errBody = LB.fromStrict $ TE.encodeUtf8 $ "No stripe customer with this id found in the database: " <> customerId_}
        Just (Entity _ sc) -> pure $ stripeCustomerUser sc

      -- If the subscription has ended, use that date instead.
      let endtime = case subscriptionEndedAt subscription of
            Just (NonNull end) -> end
            _ -> subscriptionCurrentPeriodEnd subscription

      let end = posixSecondsToUTCTime $ fromIntegral endtime
      void $
        runDb $
          upsertBy
            (UniqueSubscriptionUser uid)
            (Tickler.Subscription {subscriptionUser = uid, subscriptionEnd = end})
            [SubscriptionEnd =. end]
    Just otherProduct -> logInfoNS "stripe-hook" $ "Not fulfilling subscription for other product: " <> TE.decodeUtf8 (LB.toStrict (JSON.encodePretty otherProduct))
