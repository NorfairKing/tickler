{-# LANGUAGE TemplateHaskell #-}

module Tickler.Web.Server.Handler.Error
  ( getErrorAPIDownR,
  )
where

import Import
import Tickler.Web.Server.Foundation
import Yesod

getErrorAPIDownR :: Text -> Handler Html
getErrorAPIDownR e = withNavBar $(widgetFile "api-down")
