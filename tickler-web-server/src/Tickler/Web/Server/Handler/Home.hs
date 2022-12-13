{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Tickler.Web.Server.Handler.Home
  ( getHomeR,
  )
where

import Import
import Tickler.Client
import Tickler.Web.Server.Foundation
import Yesod

getHomeR :: Handler Html
getHomeR = do
  mPricing <- runClientOrErr clientGetPricing
  withNavBar $ do
    setTitle "Tickler"
    setDescriptionIdemp "Your GTD Tickler system; Tickler puts things back into your intray in the future."
    $(widgetFile "home")
