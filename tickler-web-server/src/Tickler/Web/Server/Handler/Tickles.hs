{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}

module Tickler.Web.Server.Handler.Tickles
  ( getTicklesR
  , makeItemInfoWidget
  ) where

import Import

import Data.Time

import Yesod

import Tickler.API
import Tickler.Client

import Tickler.Web.Server.Foundation
import Tickler.Web.Server.Time

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
    fmap mconcat $
      forM (sortByScheduled items) $ \ItemInfo {..} -> do
        createdWidget <- makeTimestampWidget itemInfoCreated
        scheduledWidget <-
          makeTimestampWidget $
          localTimeToUTC accountSettingsTimeZone $
          LocalTime
            (tickleScheduledDay itemInfoContents)
            (fromMaybe midnight $ tickleScheduledTime itemInfoContents)
        mTriggeredWidget <-
          case itemInfoTriggered of
            Nothing -> pure Nothing
            Just iit -> Just <$> makeTimestampWidget (triggeredInfoTriggered iit)
        pure $(widgetFile "tickle")

sortByScheduled :: [ItemInfo a] -> [ItemInfo a]
sortByScheduled =
  sortOn $ \tt ->
    let Tickle {..} = itemInfoContents tt
     in LocalTime tickleScheduledDay $ fromMaybe midnight tickleScheduledTime
