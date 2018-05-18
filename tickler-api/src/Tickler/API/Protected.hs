{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Tickler.API.Protected
    ( TicklerProtectedAPI
    , TicklerProtectedSite(..)
    , AuthCookie(..)
    , GetItemUUIDs
    , GetItems
    , GetShowItem
    , GetTicklerSize
    , PostAddItem
    , GetItem
    , DeleteItem
    , ItemType(..)
    , TypedItem(..)
    , textTypedItem
    , TypedItemCase(..)
    , typedItemCase
    , ItemInfo(..)
    , SyncRequest(..)
    , NewSyncItem(..)
    , SyncResponse(..)
    , PostSync
    , AccountInfo(..)
    , GetAccountInfo
    , DeleteAccount
    , Registration(..)
    , LoginForm(..)
    , GetDocsResponse(..)
    , HashedPassword
    , passwordHash
    , validatePassword
    , ItemUUID
    , AccountUUID
    , Username
    , parseUsername
    , parseUsernameWithError
    , usernameText
    ) where

import Import

import Servant.API
import Servant.Auth.Docs ()
import Servant.Auth.Server.SetCookieOrphan ()
import Servant.Generic

import Tickler.Data

import Tickler.API.Account.Types
import Tickler.API.Protected.Types
import Tickler.API.Types

type TicklerProtectedAPI = ToServant (TicklerProtectedSite AsApi)

data TicklerProtectedSite route = TicklerProtectedSite
    { getShowItem :: route :- GetShowItem
    , getTicklerSize :: route :- GetTicklerSize
    , getItemUUIDs :: route :- GetItemUUIDs
    , getItems :: route :- GetItems
    , postAddItem :: route :- PostAddItem
    , getItem :: route :- GetItem
    , deleteItem :: route :- DeleteItem
    , postSync :: route :- PostSync
    , getAccountInfo :: route :- GetAccountInfo
    , deleteAccount :: route :- DeleteAccount
    } deriving (Generic)

-- | The item is not guaranteed to be the same one for every call if there are multiple items available.
type GetShowItem
     = ProtectAPI :> "tickler" :> "show-item" :> Get '[ JSON] (Maybe (ItemInfo TypedItem))

-- | Show the number of items in the tickler
type GetTicklerSize = ProtectAPI :> "tickler" :> "size" :> Get '[ JSON] Int

-- | The order of the items is not guaranteed to be the same for every call.
type GetItemUUIDs = ProtectAPI :> "tickler" :> "uuids" :> Get '[ JSON] [ItemUUID]

-- | The order of the items is not guaranteed to be the same for every call.
type GetItems
     = ProtectAPI :> "tickler" :> "items" :> Get '[ JSON] [ItemInfo TypedItem]

type PostAddItem
     = ProtectAPI :> "tickler" :> "item" :> ReqBody '[ JSON] TypedItem :> Post '[ JSON] ItemUUID

type GetItem
     = ProtectAPI :> "tickler" :> "item" :> Capture "id" ItemUUID :> Get '[ JSON] (ItemInfo TypedItem)

type DeleteItem
     = ProtectAPI :> "item" :> Capture "id" ItemUUID :> Delete '[ JSON] NoContent

type PostSync
     = ProtectAPI :> "sync" :> ReqBody '[ JSON] SyncRequest :> Post '[ JSON] SyncResponse

type GetAccountInfo = ProtectAPI :> "account" :> Get '[ JSON] AccountInfo

type DeleteAccount = ProtectAPI :> "account" :> Delete '[ JSON] NoContent
