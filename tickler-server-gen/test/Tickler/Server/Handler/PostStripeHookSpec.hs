{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Tickler.Server.Handler.PostStripeHookSpec (spec) where

import Data.Aeson as JSON
import Data.Time
import Data.Time.Clock.POSIX
import Database.Persist
import StripeClient as Stripe
import Test.Syd.Persistent
import TestImport
import Tickler.API.Gen ()
import Tickler.Client
import Tickler.Data
import Tickler.Server.Handler.PostInitiateStripeCheckoutSession
import Tickler.Server.TestUtils

spec :: Spec
spec =
  withPaidTicklerServerAndDatabase 5 $
    it "can set the subscription of a user" $ \(pool, cenv) ->
      let eventTypes = ["customer.subscription.created", "customer.subscription.updated", "customer.subscription.deleted"]
       in forAll (elements eventTypes) $ \eventType ->
            forAllValid $ \customerId ->
              let customerForms =
                    [ SubscriptionCustomer'Text customerId,
                      SubscriptionCustomer'Customer $
                        mkCustomer
                          0
                          customerId
                          False
                    ]
               in forAll (elements customerForms) $ \customer ->
                    forAllValid $ \created ->
                      forAllValid $ \periodStart ->
                        forAllValid $ \mExistingEnd ->
                          forAllValid $ \expectedEnd ->
                            withValidNewUserAndData cenv $ \username _ token -> do
                              runPersistentTest pool $ do
                                mUser <- getBy $ UniqueUsername username
                                case mUser of
                                  Nothing -> liftIO $ expectationFailure "should have found the user."
                                  Just (Entity _ User {..}) -> do
                                    insert_ StripeCustomer {stripeCustomerUser = userIdentifier, stripeCustomerCustomer = customerId}
                                    forM_ (mExistingEnd :: Maybe UTCTime) $ \existingEnd ->
                                      insert_ Tickler.Data.Subscription {subscriptionUser = userIdentifier, subscriptionEnd = existingEnd}
                              let subscription =
                                    Stripe.Subscription
                                      { subscriptionApplication = Nothing,
                                        subscriptionApplicationFeePercent = Nothing,
                                        subscriptionAutomaticTax = JSON.object mempty,
                                        subscriptionBillingCycleAnchor = 0,
                                        subscriptionBillingThresholds = Nothing,
                                        subscriptionCancelAt = Nothing,
                                        subscriptionCancelAtPeriodEnd = False,
                                        subscriptionCanceledAt = Nothing,
                                        subscriptionCollectionMethod = SubscriptionCollectionMethod'EnumChargeAutomatically,
                                        subscriptionCreated = round $ utcTimeToPOSIXSeconds created,
                                        subscriptionCurrentPeriodEnd = round $ utcTimeToPOSIXSeconds expectedEnd,
                                        subscriptionCurrentPeriodStart = round $ utcTimeToPOSIXSeconds periodStart,
                                        subscriptionCurrency = "CHF",
                                        subscriptionCustomer = customer,
                                        subscriptionDaysUntilDue = Nothing,
                                        subscriptionDefaultPaymentMethod = Nothing,
                                        subscriptionDefaultSource = Nothing,
                                        subscriptionDefaultTaxRates = Nothing,
                                        subscriptionDescription = Nothing,
                                        subscriptionDiscount = Nothing,
                                        subscriptionEndedAt = Nothing,
                                        subscriptionId = "foobar",
                                        subscriptionItems = mkSubscriptionItems' [] False "url",
                                        subscriptionLatestInvoice = Nothing,
                                        subscriptionLivemode = False,
                                        subscriptionMetadata = metadata,
                                        subscriptionNextPendingInvoiceItemInvoice = Nothing,
                                        subscriptionPauseCollection = Nothing,
                                        subscriptionPaymentSettings = Nothing,
                                        subscriptionPendingInvoiceItemInterval = Nothing,
                                        subscriptionPendingSetupIntent = Nothing,
                                        subscriptionPendingUpdate = Nothing,
                                        subscriptionSchedule = Nothing,
                                        subscriptionStartDate = 0,
                                        subscriptionStatus = SubscriptionStatus'EnumActive,
                                        subscriptionTestClock = Nothing,
                                        subscriptionTransferData = Nothing,
                                        subscriptionTrialEnd = Nothing,
                                        subscriptionTrialStart = Nothing
                                      }
                              let JSON.Object dataObject = toJSON subscription
                              let notificationEventData = mkNotificationEventData dataObject
                              let event = mkEvent 0 notificationEventData "foobar" False 0 eventType
                              AccountInfo {..} <- runClientOrError cenv $ do
                                NoContent <- clientPostStripeHook $ toJSON event
                                clientGetAccountInfo token
                              case accountInfoStatus of
                                HasPaid actualEnd -> actualEnd `shouldBe` expectedEnd
                                _ -> expectationFailure "expected to have paid by now"
