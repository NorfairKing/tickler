{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Tickler.Web.Server.Handler.Admin
  ( getAdminR
  , postAdminAccountDeleteR
  ) where

import Import

import Data.Time
import Text.Time.Pretty

import Yesod

import Tickler.API
import Tickler.Client

import Tickler.Web.Server.Foundation
import Tickler.Web.Server.Time

getAdminR :: Handler Html
getAdminR =
  withAdminCreds $ \t -> do
    mPricing <- runClientOrErr clientGetPricing
    AdminStats {..} <- runClientOrErr $ clientAdminGetStats t
    users <- runClientOrErr $ clientAdminGetAccounts t
    now <- liftIO getCurrentTime
    token <- genToken
    withNavBar $(widgetFile "admin")

postAdminAccountDeleteR :: AccountUUID -> Handler Html
postAdminAccountDeleteR uuid =
  withAdminCreds $ \t -> do
    NoContent <- runClientOrErr $ clientAdminDeleteAccount t uuid
    redirect AdminR

withAdminCreds :: (Token -> Handler Html) -> Handler Html
withAdminCreds func =
  withLogin $ \t -> do
    adminInfo <- runClientOrErr $ clientGetAccountInfo t
    if accountInfoAdmin adminInfo
      then func t
      else notFound
