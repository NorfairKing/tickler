{-# LANGUAGE RecordWildCards #-}

module Tickler.Server.Item
    ( makeTicklerItem
    , makeTriggeredItem
    , makeTicklerSynced
    , makeItemInfo
    ) where

import Import

import Data.Time

import Tickler.API
import Tickler.Data

makeTicklerItem ::
       AccountUUID -> ItemUUID -> UTCTime -> UTCTime -> TypedItem -> TicklerItem
makeTicklerItem u i cr sy TypedItem {..} =
    TicklerItem
        { ticklerItemIdentifier = i
        , ticklerItemUserId = u
        , ticklerItemType = itemType
        , ticklerItemContents = itemData
        , ticklerItemCreated = cr
        , ticklerItemSynced = sy
        , ticklerItemScheduled = itemScheduled
        }

makeTriggeredItem ::
       AccountUUID -> ItemUUID -> UTCTime -> UTCTime -> TypedItem -> TicklerItem
makeTriggeredItem u i cr sy TypedItem {..} =
    TicklerItem
        { ticklerItemIdentifier = i
        , ticklerItemUserId = u
        , ticklerItemType = itemType
        , ticklerItemContents = itemData
        , ticklerItemCreated = cr
        , ticklerItemSynced = sy
        , ticklerItemScheduled = itemScheduled
        }

makeTicklerSynced :: TicklerItem -> Synced ItemUUID TypedItem
makeTicklerSynced TicklerItem {..} =
    Synced
        { syncedUuid = ticklerItemIdentifier
        , syncedValue =
              TypedItem
                  { itemType = ticklerItemType
                  , itemData = ticklerItemContents
                  , itemScheduled = ticklerItemScheduled
                  }
        , syncedCreated = ticklerItemCreated
        , syncedSynced = ticklerItemSynced
        }

makeItemInfo :: Either TicklerItem TriggeredItem -> ItemInfo TypedItem
makeItemInfo (Left TicklerItem {..}) =
    ItemInfo
        { itemInfoIdentifier = ticklerItemIdentifier
        , itemInfoContents =
              TypedItem
                  { itemType = ticklerItemType
                  , itemData = ticklerItemContents
                  , itemScheduled = ticklerItemScheduled
                  }
        , itemInfoCreated = ticklerItemCreated
        , itemInfoScheduled = ticklerItemScheduled
        , itemInfoTriggered = False
        }
makeItemInfo (Right TriggeredItem {..}) =
    ItemInfo
        { itemInfoIdentifier = triggeredItemIdentifier
        , itemInfoContents =
              TypedItem
                  { itemType = triggeredItemType
                  , itemData = triggeredItemContents
                  , itemScheduled = triggeredItemScheduled
                  }
        , itemInfoCreated = triggeredItemCreated
        , itemInfoScheduled = triggeredItemScheduled
        , itemInfoTriggered = True
        }
