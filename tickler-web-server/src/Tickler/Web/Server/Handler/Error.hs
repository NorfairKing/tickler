{-# LANGUAGE TemplateHaskell #-}

module Tickler.Web.Server.Handler.Error
  ( getErrorAPIDownR
  ) where

import Import

import Yesod

import Tickler.Web.Server.Foundation

getErrorAPIDownR :: Text -> Handler Html
getErrorAPIDownR e = withNavBar $(widgetFile "api-down")
