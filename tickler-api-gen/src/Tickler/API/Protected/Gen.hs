{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-dodgy-exports #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Tickler.API.Protected.Gen where

import qualified Data.Text.Encoding as TE
import Import
import Intray.Data.Gen ()
import Tickler.API.Protected.Types
import Tickler.Data.Gen ()

instance GenValid TypedItem where
  genValid = do
    ti <- genValid
    case ti of
      TextItem -> do
        t <- genValid
        pure TypedItem {itemType = TextItem, itemData = TE.encodeUtf8 t}
  shrinkValid = shrinkValidStructurally

instance GenValid a => GenValid (Tickle a)

instance GenValid a => GenValid (ItemInfo a)

instance GenValid a => GenValid (TriggerInfo a)

instance GenValid TypedTriggerInfo

instance GenValid IntrayTriggerInfo

instance GenValid EmailTriggerInfo

instance GenValid AddIntrayTrigger

instance GenValid AddEmailTrigger
