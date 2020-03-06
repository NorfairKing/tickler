{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Tickler.Web.Server.Handler.Account
  ( getAccountR
  , postAccountSettingsR
  , postAccountDeleteR
  ) where

import Import

import qualified Data.Text as T
import Data.Time

import Yesod
import Yesod.Auth

import Tickler.Client

import Tickler.Web.Server.Foundation
import Tickler.Web.Server.Time

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
    AccountInfo {..} <- runClientOrErr $ clientGetAccountInfo t
    as <- runClientOrErr $ clientGetAccountSettings t
    timestampWidget <- makeTimestampWidget accountInfoCreated
    token <- genToken
    (accountSettingsFormWidget, formEnctype) <- generateFormPost $ accountSettingsForm $ Just as
    maybe withNavBar withFormResultNavBar mfr $(widgetFile "account")

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
