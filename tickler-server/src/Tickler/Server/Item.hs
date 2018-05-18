{-# LANGUAGE RecordWildCards #-}

module Tickler.Server.Item
    ( makeTicklerItem
    , makeItemInfo
    ) where

import Data.Time

import Tickler.API
import Tickler.Data

makeTicklerItem ::
       AccountUUID -> ItemUUID -> UTCTime -> UTCTime -> TypedItem -> TicklerItem
makeTicklerItem u i cr sch TypedItem {..} =
    TicklerItem
    { ticklerItemIdentifier = i
    , ticklerItemUserId = u
    , ticklerItemType = itemType
    , ticklerItemContents = itemData
    , ticklerItemCreated = cr
    , ticklerItemScheduled = sch
    }

makeItemInfo :: TicklerItem -> ItemInfo TypedItem
makeItemInfo TicklerItem {..} =
    ItemInfo
    { itemInfoIdentifier = ticklerItemIdentifier
    , itemInfoContents =
          TypedItem {itemType = ticklerItemType, itemData = ticklerItemContents}
    , itemInfoCreated = ticklerItemCreated
    , itemInfoScheduled = ticklerItemScheduled
    }
