{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Tickler.Web.Server.Handler.Tickles
  ( getTicklesR,
    makeItemInfoWidget,
  )
where

import Data.Time
import Import
import Tickler.API
import Tickler.Client
import Tickler.Web.Server.Foundation
import Tickler.Web.Server.Time
import Yesod

getTicklesR :: Handler Html
getTicklesR = withLogin $ \t -> do
  items <- runClientOrErr $ clientGetItems t
  mItemsWidget <-
    case items of
      [] -> pure Nothing
      _ -> Just <$> makeItemInfoWidget items
  let nrItems = length items
  withNavBar $(widgetFile "tickles")

makeItemInfoWidget :: [ItemInfo] -> Handler Widget
makeItemInfoWidget items = withLogin $ \t -> do
  AccountSettings {..} <- runClientOrErr $ clientGetAccountSettings t
  token <- genToken
  now <- liftIO getCurrentTime
  fmap mconcat
    $ forM (sortByScheduled items)
    $ \ItemInfo {..} -> do
      let createdWidget = makeTimestampWidget now itemInfoCreated
      let scheduledWidget =
            makeTimestampWidget now
              $ localTimeToUTC accountSettingsTimeZone
              $ LocalTime
                (tickleScheduledDay itemInfoContents)
                (maybe midnight minuteOfDayToTimeOfDay $ tickleScheduledTime itemInfoContents)
      pure $(widgetFile "tickle")

sortByScheduled :: [ItemInfo] -> [ItemInfo]
sortByScheduled = sortOn $ \tt ->
  let Tickle {..} = itemInfoContents tt
   in LocalTime tickleScheduledDay
        $ maybe midnight minuteOfDayToTimeOfDay tickleScheduledTime
