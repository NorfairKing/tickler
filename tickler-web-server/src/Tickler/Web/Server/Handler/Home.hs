{-# LANGUAGE TemplateHaskell #-}

module Tickler.Web.Server.Handler.Home
  ( getHomeR
  ) where

import Import

import Yesod

import Tickler.Client
import Tickler.Web.Server.Foundation
import Tickler.Web.Server.Handler.Pricing

getHomeR :: Handler Html
getHomeR = do
  mPricing <- runClientOrErr clientGetPricing
  withNavBar $(widgetFile "home")
