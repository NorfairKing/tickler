{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}

module Tickler.Web.Server.Handler.Tickles
    ( getTicklesR
    ) where

import Import

import Yesod

import Tickler.API
import Tickler.Client

import Tickler.Web.Server.Foundation
import Tickler.Web.Server.Time

getTicklesR :: Handler Html
getTicklesR =
    withLogin $ \t -> do
        items <- runClientOrErr $ clientGetItems t (Just OnlyUntriggered)
        mItemWidget <-
            case items of
                [] -> pure Nothing
                _ -> Just <$> makeItemInfoWidget items
        let nrItems = length items
        withNavBar $(widgetFile "tickles")

makeItemInfoWidget :: [ItemInfo TypedItem] -> Handler Widget
makeItemInfoWidget items = do
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
