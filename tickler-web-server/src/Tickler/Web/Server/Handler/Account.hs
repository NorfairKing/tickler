{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Tickler.Web.Server.Handler.Account
  ( getAccountR,
    getAccountSettingsR,
    postAccountSettingsR,
    postAccountDeleteR,
  )
where

import qualified Data.Text as T
import Data.Time
import Import
import Tickler.Client
import Tickler.Web.Server.Foundation
import Tickler.Web.Server.Time
import Yesod
import Yesod.Auth

getAccountR :: Handler Html
getAccountR =
  withLogin $ \t -> do
    mai <- runClientOrDisallow $ clientGetAccountInfo t
    mPricing <- runClientOrErr clientGetPricing
    accountInfoWidget <- accountInfoSegment mai mPricing
    withNavBar $(widgetFile "account")

accountInfoSegment :: Maybe AccountInfo -> Maybe Pricing -> Handler Widget
accountInfoSegment mai mp =
  case mai of
    Nothing ->
      pure
        [whamlet|
          <div .ui .negative .message>
              You are not authorised to view account info.
              |]
    Just AccountInfo {..} -> do
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
              HasNotPaid _ -> maybe mempty pricingStripeForm mp
              HasPaid _ -> mempty -- already bubscribed
              NoPaymentNecessary -> mempty
          ]

pricingStripeForm :: Pricing -> Widget
pricingStripeForm p =
  [whamlet|
        <div .ui .segment>
          <h2> Subscribe

          <p>
            <ul>
              <li>
                #{pricingPrice p} per year
              <li>
                Unlimited items
              <li>
                Full API access
          <p>
            <a .button .is-primary href=@{CheckoutR}>
              Checkout
    |]

adminSegment :: Maybe AccountInfo -> Widget
adminSegment mai =
  case mai of
    Nothing -> mempty
    Just AccountInfo {..}
      | accountInfoAdmin ->
          [whamlet|
            <div .columns .is-centered>
              <div .column .is-half .content>
                <h3>
                  Admin
                <p>
                  This account is an administrator.
                <p>
                  <a .ui .positive .button href=@{AdminR AdminPanelR}>
                    The Admin Panel|]
      | otherwise -> mempty

accountSettingsForm :: FormInput Handler AccountSettings
accountSettingsForm =
  AccountSettings
    <$> ireq
      ( selectField . pure $
          mkOptionList $ do
            tz <- timeZoneChoices
            pure
              Option
                { optionDisplay = T.pack $ timeZoneName tz,
                  optionInternalValue = tz,
                  optionExternalValue = T.pack $ show $ timeZoneMinutes tz
                }
      )
      "timezone"

getAccountSettingsR :: Handler Html
getAccountSettingsR = withLogin $ \t -> do
  as <- runClientOrErr $ clientGetAccountSettings t
  token <- genToken
  withNavBar $(widgetFile "account/settings")

postAccountSettingsR :: Handler Html
postAccountSettingsR =
  withLogin $ \t -> do
    as <- runInputPost accountSettingsForm
    NoContent <- runClientOrErr $ clientPutAccountSettings t as
    addPositiveMessage "Account Settings Saved"
    redirect AccountSettingsR

postAccountDeleteR :: Handler Html
postAccountDeleteR =
  withLogin $ \t -> do
    NoContent <- runClientOrErr $ clientDeleteAccount t
    clearCreds False
    redirect HomeR
