{-# LANGUAGE RecordWildCards #-}

module Tickler.Server.Item
    ( makeTicklerItem
    , makeItemInfo
    ) where

import Data.Time

import Tickler.API
import Tickler.Data

makeTicklerItem :: AccountUUID -> ItemUUID -> UTCTime -> TypedItem -> TicklerItem
makeTicklerItem u i ts TypedItem {..} =
    TicklerItem
    { ticklerItemIdentifier = i
    , ticklerItemType = itemType
    , ticklerItemContents = itemData
    , ticklerItemTimestamp = ts
    , ticklerItemUserId = u
    }

makeItemInfo :: TicklerItem -> ItemInfo TypedItem
makeItemInfo TicklerItem {..} =
    ItemInfo
    { itemInfoIdentifier = ticklerItemIdentifier
    , itemInfoContents =
          TypedItem {itemType = ticklerItemType, itemData = ticklerItemContents}
    , itemInfoTimestamp = ticklerItemTimestamp
    }
