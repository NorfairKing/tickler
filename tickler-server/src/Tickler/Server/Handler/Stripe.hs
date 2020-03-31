{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}

module Tickler.Server.Handler.Stripe
  ( runStripeHandler
  , runStripeHandlerOrError
  , runStripeHandlerOrErrorWith
  , PaidStatus(..)
  , getUserPaidStatus
  ) where

import Import

import Control.Exception

import Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as LB8
import Data.Ord
import Data.Time
import Database.Persist

import Servant
import Servant.Auth.Server

import Web.Stripe as Stripe (StripeError, StripeRequest, StripeReturn)
import qualified Web.Stripe.Subscription as Stripe
import qualified Web.Stripe.Types as Stripe

import Tickler.API

import Tickler.Server.Handler.Utils
import Tickler.Server.OptParse.Types
import Tickler.Server.Stripe
import Tickler.Server.Types

runStripeHandler ::
     FromJSON (StripeReturn a)
  => StripeRequest a
  -> TicklerHandler (Maybe (Either StripeError (StripeReturn a)))
runStripeHandler request = do
  mStripeSets <- asks (fmap monetisationEnvStripeSettings . envMonetisation)
  forM mStripeSets $ \ms -> liftIO $ runStripeWith ms request

runStripeHandlerOrError ::
     FromJSON (StripeReturn a) => StripeRequest a -> TicklerHandler (Maybe (StripeReturn a))
runStripeHandlerOrError request = do
  mStripeSets <- asks (fmap monetisationEnvStripeSettings . envMonetisation)
  forM mStripeSets $ \ms -> runStripeHandlerOrErrorWith ms request

runStripeHandlerOrErrorWith ::
     FromJSON (StripeReturn a)
  => StripeSettings
  -> StripeRequest a
  -> TicklerHandler (StripeReturn a)
runStripeHandlerOrErrorWith ms request = do
  errOrRes <- liftIO $ runStripeWith ms request
  case errOrRes of
    Left err -> throwError (err503 {errBody = LB8.pack $ displayException err})
    Right res -> pure res

getUserPaidStatus :: AccountUUID -> TicklerHandler PaidStatus
getUserPaidStatus userId = do
  mss <- asks envMonetisation
  case mss of
    Nothing -> pure NoPaymentNecessary
    Just MonetisationEnv {..} -> do
      mu <- runDb $ getBy $ UniqueUserIdentifier userId
      case mu of
        Nothing -> throwAll err404
        Just (Entity _ User {..}) -> do
          isAdmin <- asks ((userUsername `elem`) . envAdmins)
          if isAdmin
            then pure NoPaymentNecessary
            else do
              mSub <- hasSubscribed monetisationEnvStripeSettings userId
              case mSub of
                Just u -> pure $ HasPaid u
                Nothing -> do
                  c <- runDb $ count [TicklerItemUserId ==. userId]
                  pure $ HasNotPaid (monetisationEnvMaxItemsFree - c)

hasSubscribed :: StripeSettings -> AccountUUID -> TicklerHandler (Maybe UTCTime)
hasSubscribed ss uuid = do
  mc <- runDb $ getBy $ UniqueCustomerUser uuid
  case mc of
    Nothing -> pure Nothing -- No such customer on the stripe end, definitely hasn't subscribed then.
    Just (Entity _ Customer {..}) -> do
      sl <-
        runStripeHandlerOrErrorWith ss (Stripe.getSubscriptionsByCustomerId customerStripeCustomer)
      let relevantSubs =
            filter
              (\s ->
                 Stripe.planId (Stripe.subscriptionPlan s) == stripeSetPlan ss &&
                 (Stripe.subscriptionStatus s == Stripe.Active ||
                  Stripe.subscriptionStatus s == Stripe.Trialing)) $
            Stripe.list sl
      pure $
        case sortOn Down $ map Stripe.subscriptionCurrentPeriodEnd relevantSubs of
          [] -> Nothing
          (end:_) -> Just end
