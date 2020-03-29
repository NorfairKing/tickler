{-# LANGUAGE TemplateHaskell #-}

module Tickler.Web.Server.Time
  ( makeTimestampWidgetNow
  , makeTimestampWidget
  , prettyTimestamp
  ) where

import Import

import Data.Time
import Text.Time.Pretty

import Tickler.Web.Server.Foundation

makeTimestampWidgetNow :: UTCTime -> Handler Widget
makeTimestampWidgetNow timestamp = do
  now <- liftIO getCurrentTime
  pure $ makeTimestampWidget now timestamp

makeTimestampWidget :: UTCTime -> UTCTime -> Widget
makeTimestampWidget now timestamp =
  let timeStr = prettyTimestamp now timestamp
      timeAgoString = prettyTimeAuto now timestamp
   in $(widgetFile "timestamp")

prettyTimestamp :: UTCTime -> UTCTime -> String
prettyTimestamp now d =
  let year = (\(y, _, _) -> y) . toGregorian . utctDay
   in (if year now == year d
         then formatTime defaultTimeLocale "%A %B %e at %H:%M"
         else formatTime defaultTimeLocale "%A %B %e %Y at %H:%M")
        d
