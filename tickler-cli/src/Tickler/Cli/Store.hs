{-# LANGUAGE RecordWildCards #-}

module Tickler.Cli.Store
    ( Store(..)
    , StoreItem(..)
    , readStore
    , readStoreOrEmpty
    , writeStore
    , addItemToStore
    , LastItem(..)
    , lastItemInStore
    , doneLastItem
    , storeSize
    ) where

import Import

import Tickler.API
import Tickler.Client.Store

import Tickler.Cli.JSON
import Tickler.Cli.LastSeen
import Tickler.Cli.OptParse
import Tickler.Cli.Path

{-# ANN module "HLint: ignore Use &&" #-}

readStore :: CliM (Maybe Store)
readStore = storePath >>= readJSON

readStoreOrEmpty :: CliM Store
readStoreOrEmpty = fromMaybe emptyStore <$> readStore

writeStore :: Store -> CliM ()
writeStore s = do
    checkLastSeenAfter s
    storePath >>= (`writeJSON` s)

checkLastSeenAfter :: Store -> CliM ()
checkLastSeenAfter s = do
    mLs <- readLastSeen
    case mLs of
        Nothing -> pure () -- Nothing was last seen, cannot be out of date
        Just ls -> unless (lastSeenInStore ls s) clearLastSeen

lastSeenInStore :: LastItem -> Store -> Bool
lastSeenInStore LastItem {..} Store {..} = any matches storeItems
  where
    matches (Unsynced t ts) =
        and [lastItemTimestamp == ts, lastItemData == t, isNothing lastItemUUID]
    matches (Synced ItemInfo {..}) =
        and
            [ lastItemTimestamp == itemInfoTimestamp
            , lastItemData == itemInfoContents
            , lastItemUUID == Just itemInfoIdentifier
            ]
    matches (Undeleted _) = False -- If it's deleted locally, we should report it's not in the store, even if we haven't synced yet.
