{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}

module Tickler.Web.Server.Handler.Triggered
    ( getTriggeredR
    ) where

import Import

import Data.Time

import Yesod

import Tickler.API
import Tickler.Client

import Tickler.Web.Server.Foundation
import Tickler.Web.Server.Time

getTriggeredR :: Handler Html
getTriggeredR =
    withLogin $ \t -> do
        items <- runClientOrErr $ clientGetItems t (Just OnlyTriggered)
        mItemWidget <-
            case items of
                [] -> pure Nothing
                _ -> Just <$> makeItemInfosWidget items
        let nrItems = length items
        withNavBar $(widgetFile "triggereds")

makeItemInfosWidget :: [TypedItemInfo] -> Handler Widget
makeItemInfosWidget items =
    withLogin $ \t -> do
        AccountSettings {..} <- runClientOrErr $ clientGetAccountSettings t
        token <- genToken
        fmap mconcat $
            forM items $ \ItemInfo {..} -> do
                createdWidget <- makeTimestampWidget itemInfoCreated
                scheduledWidget <-
                    makeTimestampWidget $
                    localTimeToUTC accountSettingsTimeZone $
                    LocalTime
                        (tickleScheduledDay itemInfoContents)
                        (fromMaybe midnight $
                         tickleScheduledTime itemInfoContents)
                mTriggeredWidget <-
                    case itemInfoTriggered of
                        Nothing -> pure Nothing
                        Just iit ->
                            Just <$>
                            makeTimestampWidget (triggeredInfoTriggered iit)
                pure $(widgetFile "triggered")
