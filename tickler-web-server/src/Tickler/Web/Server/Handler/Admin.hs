{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Tickler.Web.Server.Handler.Admin
  ( getAdminPanelR,
    getAdminAccountR,
    postAdminAccountDeleteR,
    getAdminAccountSetSubscriptionR,
    postAdminAccountSetSubscriptionR,
  )
where

import Control.Exception (throwIO)
import Data.Time
import Import
import Network.HTTP.Types as HTTP
import Text.Time.Pretty
import Tickler.API
import Tickler.Client
import Tickler.Web.Server.Foundation
import Tickler.Web.Server.Time
import Yesod

getAdminPanelR :: Handler Html
getAdminPanelR =
  withAdminCreds $ \t -> do
    mPricing <- runClientOrErr clientGetPricing
    AdminStats {..} <- runClientOrErr $ clientAdminGetStats t
    users <- runClientOrErr $ clientAdminGetAccounts t
    now <- liftIO getCurrentTime
    token <- genToken
    let ActiveUsers {..} = adminStatsActiveUsers
    withNavBar $(widgetFile "admin/panel")

getAdminAccountR :: Username -> Handler Html
getAdminAccountR username = withAdminCreds $ \t -> withUserViaAdmin t username $ \AccountInfo {..} -> do
  now <- liftIO getCurrentTime
  withNavBar $ do
    token <- genToken
    $(widgetFile "admin/account")

postAdminAccountDeleteR :: Username -> Handler Html
postAdminAccountDeleteR username =
  withAdminCreds $ \t -> do
    NoContent <- runClientOrErr $ clientAdminDeleteAccount t username
    redirect $ AdminR AdminPanelR

getAdminAccountSetSubscriptionR :: Username -> Handler Html
getAdminAccountSetSubscriptionR username = withAdminCreds $ \t -> withUserViaAdmin t username $ \AccountInfo {..} -> do
  now <- liftIO getCurrentTime
  withNavBar $ do
    token <- genToken
    $(widgetFile "admin/account/set-subscription")

postAdminAccountSetSubscriptionR :: Username -> Handler Html
postAdminAccountSetSubscriptionR username = withAdminCreds $ \t -> do
  endDate <- runInputPost $ ireq dayField "end-date"
  NoContent <- runClientOrErr $ clientAdminPutAccountSubscription t username $ UTCTime endDate 0
  redirect $ AdminR $ AdminAccountSetSubscriptionR username

withAdminCreds :: (Token -> Handler a) -> Handler a
withAdminCreds func =
  withLogin $ \t -> do
    adminInfo <- runClientOrErr $ clientGetAccountInfo t
    if accountInfoAdmin adminInfo
      then func t
      else notFound

withUserViaAdmin :: Token -> Username -> (AccountInfo -> Handler a) -> Handler a
withUserViaAdmin t username func = do
  errOrAccountInfo <- runClient $ clientAdminGetAccount t username
  case errOrAccountInfo of
    Left err -> case err of
      FailureResponse _ resp ->
        if HTTP.statusCode (responseStatusCode resp) == 404
          then notFound
          else liftIO $ throwIO err
      _ -> liftIO $ throwIO err
    Right accountInfo -> func accountInfo
