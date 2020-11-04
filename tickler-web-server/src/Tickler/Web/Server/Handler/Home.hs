{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Tickler.Web.Server.Handler.Home
  ( getHomeR
  ) where

import Import
import Tickler.Client
import Tickler.Web.Server.Foundation
import Tickler.Web.Server.Handler.Pricing
import Yesod

getHomeR :: Handler Html
getHomeR = do
  mPricing <- runClientOrErr clientGetPricing
  withNavBar $ do
    setTitle "Tickler"
    setDescription
      "Your GTD Tickler system; Tickler puts things back into your intray in the future."
    $(widgetFile "home")
