{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Tickler.Web.Server.Handler.Account
  ( getAccountR,
    postAccountSettingsR,
    postAccountDeleteR,
  )
where

import qualified Data.Text as T
import Data.Time
import Import
import Text.Julius
import Tickler.Client
import Tickler.Web.Server.Foundation
import Tickler.Web.Server.Handler.Pricing
import Tickler.Web.Server.Time
import Web.Stripe.Plan as Stripe
import Yesod
import Yesod.Auth

accountSettingsForm ::
  Maybe AccountSettings -> Html -> MForm Handler (FormResult AccountSettings, Widget)
accountSettingsForm mas extra = do
  (timeZoneRes, timeZoneView) <-
    mreq
      (selectFieldList $ map (\tz -> (T.pack $ timeZoneName tz, tz)) timeZoneChoices)
      "Not Used"
      (dv accountSettingsTimeZone)
  let accountSettingsRes = AccountSettings <$> timeZoneRes
  let accountSettingsWidget =
        [whamlet|
                #{extra}
                <div .field>
                    <label>Time Zone</label>
                    ^{fvInput timeZoneView}
            |]
  return (accountSettingsRes, accountSettingsWidget)
  where
    dv :: (AccountSettings -> a) -> Maybe a
    dv func = func <$> mas

getAccountPage :: Maybe (FormResult a) -> Handler Html
getAccountPage mfr =
  withLogin $ \t -> do
    mai <- runClientOrDisallow $ clientGetAccountInfo t
    mPricing <- runClientOrErr clientGetPricing
    accountInfoWidget <- accountInfoSegment mai mPricing
    as <- runClientOrErr $ clientGetAccountSettings t
    token <- genToken
    (accountSettingsFormWidget, formEnctype) <- generateFormPost $ accountSettingsForm $ Just as
    maybe withNavBar withFormResultNavBar mfr $(widgetFile "account")

accountInfoSegment :: Maybe AccountInfo -> Maybe Pricing -> Handler Widget
accountInfoSegment mai mp =
  case mai of
    Nothing ->
      pure
        [whamlet|
          <div .ui .negative .message>
              You are not authorised to view account info.
              |]
    Just ai@AccountInfo {..} -> do
      now <- liftIO getCurrentTime
      let subbedWidget =
            case accountInfoStatus of
              HasNotPaid _ -> [whamlet|Not subscribed|]
              HasPaid subbed -> [whamlet|Subscribed until ^{makeTimestampWidget now subbed}|]
              NoPaymentNecessary -> [whamlet|No payment necessary|]
          createdWidget = makeTimestampWidget now accountInfoCreated
      pure $
        mconcat
          [ [whamlet|
          <div .ui .segment>
            <h3>
              Info
            <p> Username: #{usernameText accountInfoUsername}
            <p> Created: ^{createdWidget}
            $maybe _ <- mp
              <p>
                Status: ^{subbedWidget}
          |],
            case accountInfoStatus of
              HasNotPaid _ -> maybe mempty (pricingStripeForm ai) mp
              HasPaid _ -> mempty -- already bubscribed
              NoPaymentNecessary -> mempty
          ]

pricingStripeForm :: AccountInfo -> Pricing -> Widget
pricingStripeForm AccountInfo {..} p =
  let Stripe.PlanId planText = pricingPlan p
      clientReferenceId = uuidText accountInfoUUID
      sf = $(widgetFile "stripe-form")
   in [whamlet|
        <div .ui .segment>
          <h2> Subscribe

          <p>
            <ul>
              <li>
                #{pricingShowAmountPerMonth p} per month, billed anually
              <li>
                Unlimited items
              <li>
                Full API access
              $maybe tp <- pricingTrialPeriod p
                <li>
                  #{show tp} day Trial period
          <p>
            ^{sf}
    |]

adminSegment :: Maybe AccountInfo -> Widget
adminSegment mai =
  case mai of
    Nothing -> mempty
    Just AccountInfo {..}
      | accountInfoAdmin ->
        [whamlet|
            <div .ui .segment>
                <h3>
                  Admin
                <p>
                  This account is an administrator.
                <p>
                  <a .ui .positive .button href=@{AdminR}>
                    The Admin Panel|]
      | otherwise -> mempty

getAccountR :: Handler Html
getAccountR = getAccountPage Nothing

postAccountSettingsR :: Handler Html
postAccountSettingsR =
  withLogin $ \t -> do
    ((result, _), _) <- runFormPost $ accountSettingsForm Nothing
    case result of
      FormSuccess as -> do
        NoContent <- runClientOrErr $ clientPutAccountSettings t as
        addMessage "success" "Account Settings Saved"
        redirect AccountR
      fr -> getAccountPage $ Just fr

postAccountDeleteR :: Handler Html
postAccountDeleteR =
  withLogin $ \t -> do
    NoContent <- runClientOrErr $ clientDeleteAccount t
    clearCreds False
    redirect HomeR
