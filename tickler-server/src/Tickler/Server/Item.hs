{-# LANGUAGE RecordWildCards #-}

module Tickler.Server.Item
  ( makeTicklerItem,
    makeTicklerItemInfo,
  )
where

import Data.Time
import Tickler.API

makeTicklerItem ::
  AccountUUID -> ItemUUID -> UTCTime -> Tickle -> TicklerItem
makeTicklerItem u i cr Tickle {..} =
  TicklerItem
    { ticklerItemIdentifier = i,
      ticklerItemUserId = u,
      ticklerItemContents = tickleContent,
      ticklerItemCreated = cr,
      ticklerItemScheduledDay = tickleScheduledDay,
      ticklerItemScheduledTime = tickleScheduledTime,
      ticklerItemRecurrence = tickleRecurrence
    }

makeTicklerItemInfo :: TicklerItem -> ItemInfo
makeTicklerItemInfo TicklerItem {..} =
  ItemInfo
    { itemInfoIdentifier = ticklerItemIdentifier,
      itemInfoContents =
        Tickle
          { tickleContent = ticklerItemContents,
            tickleScheduledDay = ticklerItemScheduledDay,
            tickleScheduledTime = ticklerItemScheduledTime,
            tickleRecurrence = ticklerItemRecurrence
          },
      itemInfoCreated = ticklerItemCreated
    }
