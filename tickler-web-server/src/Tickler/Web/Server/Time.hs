{-# LANGUAGE TemplateHaskell #-}

module Tickler.Web.Server.Time
    ( makeTimestampWidget
    , prettyTimestamp
    ) where

import Import

import Data.Time
import Text.Time.Pretty


import Tickler.Web.Server.Foundation

makeTimestampWidget :: UTCTime -> Handler Widget
makeTimestampWidget timestamp = do
    now <- liftIO getCurrentTime
    let timeStr = prettyTimestamp now timestamp
    let timeAgoString = prettyTimeAuto now timestamp
    pure $(widgetFile "timestamp")

prettyTimestamp :: UTCTime -> UTCTime -> String
prettyTimestamp now d =
    let year = (\(y, _, _) -> y) . toGregorian . utctDay
    in (if year now == year d
            then formatTime defaultTimeLocale "%A %B %e at %H:%M"
            else formatTime defaultTimeLocale "%A %B %e %Y at %H:%M")
           d
