{-# LANGUAGE RecordWildCards #-}

module Tickler.Server.Item
  ( makeTicklerItem,
    makeTicklerItemInfo,
  )
where

import Data.Time
import Tickler.API

makeTicklerItem ::
  AccountUUID -> ItemUUID -> UTCTime -> TypedTickle -> TicklerItem
makeTicklerItem u i cr Tickle {..} =
  let TypedItem {..} = tickleContent
   in TicklerItem
        { ticklerItemIdentifier = i,
          ticklerItemUserId = u,
          ticklerItemType = itemType,
          ticklerItemContents = itemData,
          ticklerItemCreated = cr,
          ticklerItemScheduledDay = tickleScheduledDay,
          ticklerItemScheduledTime = tickleScheduledTime,
          ticklerItemRecurrence = tickleRecurrence
        }

makeTicklerItemInfo :: TicklerItem -> TypedItemInfo
makeTicklerItemInfo TicklerItem {..} =
  ItemInfo
    { itemInfoIdentifier = ticklerItemIdentifier,
      itemInfoContents =
        Tickle
          { tickleContent = TypedItem {itemType = ticklerItemType, itemData = ticklerItemContents},
            tickleScheduledDay = ticklerItemScheduledDay,
            tickleScheduledTime = ticklerItemScheduledTime,
            tickleRecurrence = ticklerItemRecurrence
          },
      itemInfoCreated = ticklerItemCreated
    }
