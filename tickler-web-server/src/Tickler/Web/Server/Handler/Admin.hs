{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Tickler.Web.Server.Handler.Admin
  ( getAdminPanelR,
    postAdminAccountDeleteR,
  )
where

import Data.Time
import Import
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
    withNavBar $(widgetFile "admin")

postAdminAccountDeleteR :: Username -> Handler Html
postAdminAccountDeleteR username =
  withAdminCreds $ \t -> do
    NoContent <- runClientOrErr $ clientAdminDeleteAccount t username
    redirect $ AdminR AdminPanelR

withAdminCreds :: (Token -> Handler Html) -> Handler Html
withAdminCreds func =
  withLogin $ \t -> do
    adminInfo <- runClientOrErr $ clientGetAccountInfo t
    if accountInfoAdmin adminInfo
      then func t
      else notFound
