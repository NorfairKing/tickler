{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Tickler.Web.Server.Handler.Triggered
  ( getTriggeredsR,
  )
where

import Data.Time
import Import
import Tickler.API
import Tickler.Client
import Tickler.Web.Server.Foundation
import Tickler.Web.Server.Time
import Yesod

getTriggeredsR :: Handler Html
getTriggeredsR =
  withLogin $ \t -> do
    items <- runClientOrErr $ clientGetItems t (Just OnlyTriggered)
    mItemWidget <-
      case items of
        [] -> pure Nothing
        _ -> Just <$> makeItemInfosWidget items
    let nrItems = length items
    token <- genToken
    withNavBar $(widgetFile "triggereds")

makeItemInfosWidget :: [TypedItemInfo] -> Handler Widget
makeItemInfosWidget items =
  withLogin $ \t -> do
    AccountSettings {..} <- runClientOrErr $ clientGetAccountSettings t
    token <- genToken
    now <- liftIO getCurrentTime
    fmap mconcat
      $ forM items
      $ \ItemInfo {..} -> do
        let createdWidget = makeTimestampWidget now itemInfoCreated
        let scheduledWidget =
              makeTimestampWidget now
                $ localTimeToUTC accountSettingsTimeZone
                $ LocalTime
                  (tickleScheduledDay itemInfoContents)
                  (fromMaybe midnight $ tickleScheduledTime itemInfoContents)
        let mTriggeredWidget =
              case itemInfoTriggered of
                Nothing -> Nothing
                Just iit -> Just $ makeTimestampWidget now (triggeredInfoTriggered iit)
        pure $(widgetFile "triggered")
