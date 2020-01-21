{-# LANGUAGE TemplateHaskell #-}

module Tickler.Web.Server.Handler.Home
  ( getHomeR
  ) where

import Yesod

import Tickler.Web.Server.Foundation

getHomeR :: Handler Html
getHomeR = withNavBar $(widgetFile "home")
