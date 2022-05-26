{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Tickler.Server.Handler.PostStripeHookSpec (spec) where

import Data.Aeson as JSON
import qualified Data.HashMap.Strict as HM
import Data.Time.Clock.POSIX
import Database.Persist
import Network.HTTP.Types
import StripeClient as Stripe
import Test.Syd.Persistent
import TestImport
import Tickler.API.Gen ()
import Tickler.Client
import Tickler.Data
import Tickler.Server.TestUtils

spec :: Spec
spec =
  withPaidTicklerServerAndDatabase 5 $
    it "can set the subscription of a user" $ \(pool, cenv) ->
      forAllValid $ \customerId ->
        forAllValid $ \created ->
          forAllValid $ \periodStart ->
            forAllValid $ \expectedEnd ->
              withValidNewUserAndData cenv $ \username _ token -> do
                runPersistentTest pool $ do
                  mUser <- getBy $ UniqueUsername username
                  case mUser of
                    Nothing -> liftIO $ expectationFailure "should have found the user."
                    Just (Entity _ User {..}) -> insert_ StripeCustomer {stripeCustomerUser = userIdentifier, stripeCustomerCustomer = customerId}
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
                          subscriptionCustomer =
                            SubscriptionCustomer'Customer $
                              mkCustomer
                                0
                                customerId
                                False,
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
                          subscriptionMetadata =
                            HM.fromList
                              [ ("product", "tickler")
                              ],
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
                let event = mkEvent 0 notificationEventData "foobar" False 0 "customer.subscription.created"
                AccountInfo {..} <- runClientOrError cenv $ do
                  NoContent <- clientPostStripeHook $ toJSON event
                  clientGetAccountInfo token
                case accountInfoStatus of
                  HasPaid actualEnd -> actualEnd `shouldBe` expectedEnd
                  _ -> expectationFailure "expected to have paid by now"
