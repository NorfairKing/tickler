{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}

module Tickler.Web.Server.Handler.Triggered
    ( getTriggeredR
    ) where

import Import

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
        withNavBar $(widgetFile "triggered")

makeItemInfosWidget :: [TypedItemInfo] -> Handler Widget
makeItemInfosWidget items = do
    token <- genToken
    fmap mconcat $
        forM items $ \ItemInfo {..} -> do
            createdWidget <- makeTimestampWidget itemInfoCreated
            scheduledWidget <-
                makeTimestampWidget $ tickleScheduled itemInfoContents
            mTriggeredWidget <-
                case itemInfoTriggered of
                    Nothing -> pure Nothing
                    Just iit ->Just <$> makeTimestampWidget iit
            pure $(widgetFile "item")
