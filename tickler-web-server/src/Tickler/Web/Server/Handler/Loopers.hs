{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Tickler.Web.Server.Handler.Loopers
    ( getLoopersR
    ) where

import Import

import Yesod

import Tickler.API
import Tickler.Client

import Tickler.Web.Server.Foundation

getLoopersR :: Handler Html
getLoopersR = do
    LoopersStatus {..} <- runClientOrErr clientGetLoopersStatus
    withNavBar $(widgetFile "loopers")

mkLooperStatusWidget :: Text -> LooperStatus -> Widget
mkLooperStatusWidget name ls = $(widgetFile "looper")

loopersColor :: LooperStatus -> Text
loopersColor LooperStatusDisabled = "black"
loopersColor LooperStatusRunning = "green"
loopersColor (LooperStatusErrored _) = "red"
loopersColor LooperStatusStopped = "red"
