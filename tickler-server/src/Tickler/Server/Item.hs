{-# LANGUAGE RecordWildCards #-}

module Tickler.Server.Item
    ( makeTicklerItem
    , makeItemInfo
    ) where

import Import

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

makeItemInfo :: Either TicklerItem TriggeredItem -> ItemInfo TypedItem
makeItemInfo (Left TicklerItem {..}) =
    ItemInfo
    { itemInfoIdentifier = ticklerItemIdentifier
    , itemInfoContents =
          TypedItem {itemType = ticklerItemType, itemData = ticklerItemContents}
    , itemInfoCreated = ticklerItemCreated
    , itemInfoScheduled = ticklerItemScheduled
    , itemInfoTriggered = False
    }
makeItemInfo (Right TriggeredItem {..}) =
    ItemInfo
    { itemInfoIdentifier = triggeredItemIdentifier
    , itemInfoContents =
          TypedItem
          {itemType = triggeredItemType, itemData = triggeredItemContents}
    , itemInfoCreated = triggeredItemCreated
    , itemInfoScheduled = triggeredItemScheduled
    , itemInfoTriggered = True
    }
