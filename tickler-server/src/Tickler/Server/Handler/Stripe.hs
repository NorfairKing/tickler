{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}

module Tickler.Server.Handler.Stripe
  ( runStripeHandler,
    runStripeHandlerOrError,
    runStripeHandlerOrErrorWith,
    PaidStatus (..),
    getUserPaidStatus,
  )
where

import Control.Exception
import Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as LB8
import qualified Data.List.NonEmpty as NE
import Data.Ord
import Data.Time
import Database.Persist
import Import
import Servant
import Servant.Auth.Server
import Tickler.API
import Tickler.Server.Handler.Utils
import Tickler.Server.OptParse.Types
import Tickler.Server.Stripe
import Tickler.Server.Types
import Web.Stripe as Stripe (StripeError, StripeRequest, StripeReturn)
import qualified Web.Stripe.Subscription as Stripe
import qualified Web.Stripe.Types as Stripe

runStripeHandler ::
  FromJSON (StripeReturn a) =>
  StripeRequest a ->
  TicklerHandler (Maybe (Either StripeError (StripeReturn a)))
runStripeHandler request = do
  mStripeSets <- asks (fmap monetisationEnvStripeSettings . envMonetisation)
  forM mStripeSets $ \ms -> liftIO $ runStripeWith ms request

runStripeHandlerOrError ::
  FromJSON (StripeReturn a) => StripeRequest a -> TicklerHandler (Maybe (StripeReturn a))
runStripeHandlerOrError request = do
  mStripeSets <- asks (fmap monetisationEnvStripeSettings . envMonetisation)
  forM mStripeSets $ \ms -> runStripeHandlerOrErrorWith ms request

runStripeHandlerOrErrorWith ::
  FromJSON (StripeReturn a) =>
  StripeSettings ->
  StripeRequest a ->
  TicklerHandler (StripeReturn a)
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
          isFreeloader <- asks ((userUsername `elem`) . envFreeloaders)
          if isFreeloader
            then pure NoPaymentNecessary
            else do
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
  cs <- runDb $ selectList [StripeCustomerUser ==. uuid] []
  ends <- forM cs $ \(Entity _ StripeCustomer {..}) -> do
    sl <-
      runStripeHandlerOrErrorWith ss (Stripe.getSubscriptionsByCustomerId stripeCustomerCustomer)
    let relevantSubs =
          filter
            ( \s ->
                Stripe.planId (Stripe.subscriptionPlan s) == stripeSetPlan ss
                  && ( Stripe.subscriptionStatus s == Stripe.Active
                         || Stripe.subscriptionStatus s == Stripe.Trialing
                     )
            )
            $ Stripe.list sl
    pure $
      case sortOn Down $ map Stripe.subscriptionCurrentPeriodEnd relevantSubs of
        [] -> Nothing
        (end : _) -> Just end
  let mt = do
        ne <- NE.nonEmpty $ catMaybes ends
        pure $ NE.head $ NE.sortWith Down ne

  -- Put it in our db
  forM_ mt $ \t ->
    runDb $
      upsertBy
        (UniqueSubscriptionUser uuid)
        (Subscription {subscriptionUser = uuid, subscriptionEnd = t})
        [SubscriptionEnd =. t]
  pure mt
