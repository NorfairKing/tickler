{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Tickler.Web.Server.Handler.Loopers
  ( getLoopersR,
  )
where

import Import
import Tickler.API
import Tickler.Client
import Tickler.Web.Server.Foundation
import Yesod

getLoopersR :: Handler Html
getLoopersR = do
  LoopersInfo {..} <- runClientOrErr clientGetLoopersInfo
  withNavBar $(widgetFile "loopers")

mkLooperInfoWidget :: Text -> LooperInfo -> Widget
mkLooperInfoWidget name ls = $(widgetFile "looper")

loopersColor :: LooperStatus -> Text
loopersColor LooperStatusDisabled = "black"
loopersColor LooperStatusRunning = "green"
loopersColor (LooperStatusErrored _) = "red"
loopersColor LooperStatusStopped = "red"
