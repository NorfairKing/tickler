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
getTicklesR =
  withLogin $ \t -> do
    items <- runClientOrErr $ clientGetItems t (Just OnlyUntriggered)
    mItemsWidget <-
      case items of
        [] -> pure Nothing
        _ -> Just <$> makeItemInfoWidget items
    let nrItems = length items
    withNavBar $(widgetFile "tickles")

makeItemInfoWidget :: [ItemInfo TypedItem] -> Handler Widget
makeItemInfoWidget items =
  withLogin $ \t -> do
    AccountSettings {..} <- runClientOrErr $ clientGetAccountSettings t
    token <- genToken
    now <- liftIO getCurrentTime
    fmap mconcat $
      forM (sortByScheduled items) $
        \ItemInfo {..} -> do
          let createdWidget = makeTimestampWidget now itemInfoCreated
          let scheduledWidget =
                makeTimestampWidget now $
                  localTimeToUTC accountSettingsTimeZone $
                    LocalTime
                      (tickleScheduledDay itemInfoContents)
                      (fromMaybe midnight $ tickleScheduledTime itemInfoContents)
          let mTriggeredWidget =
                case itemInfoTriggered of
                  Nothing -> Nothing
                  Just iit -> Just $ makeTimestampWidget now (triggeredInfoTriggered iit)
          pure $(widgetFile "tickle")

sortByScheduled :: [ItemInfo a] -> [ItemInfo a]
sortByScheduled =
  sortOn $ \tt ->
    let Tickle {..} = itemInfoContents tt
     in LocalTime tickleScheduledDay $ fromMaybe midnight tickleScheduledTime
