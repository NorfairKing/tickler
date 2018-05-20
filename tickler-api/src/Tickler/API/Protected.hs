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
    , PostAddItem
    , GetItem
    , DeleteItem
    , ItemFilter(..)
    , ItemType(..)
    , TypedItem(..)
    , textTypedItem
    , TypedItemCase(..)
    , typedItemCase
    , ItemInfo(..)
    , AddItem(..)
    , SyncRequest(..)
    , NewSyncItem(..)
    , SyncResponse(..)
    , PostSync
    , TriggerInfo(..)
    , GetTriggers
    , AddIntrayTrigger(..)
    , PostAddIntrayTrigger
    , AddEmailTrigger(..)
    , PostAddEmailTrigger
    , AccountInfo(..)
    , GetAccountInfo
    , AccountSettings(..)
    , GetAccountSettings
    , PutAccountSettings
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
import Servant.Docs
import Servant.Generic

import Tickler.Data

import Tickler.API.Account.Types
import Tickler.API.Protected.Types
import Tickler.API.Types

type TicklerProtectedAPI = ToServant (TicklerProtectedSite AsApi)

data TicklerProtectedSite route = TicklerProtectedSite
    { getItemUUIDs :: route :- GetItemUUIDs
    , getItems :: route :- GetItems
    , postAddItem :: route :- PostAddItem
    , getItem :: route :- GetItem
    , deleteItem :: route :- DeleteItem
    , postSync :: route :- PostSync
    , getTriggers :: route :- GetTriggers
    , postAddIntrayTrigger :: route :- PostAddIntrayTrigger
    , postAddEmailTrigger :: route :- PostAddEmailTrigger
    , getAccountInfo :: route :- GetAccountInfo
    , getAccountSettings :: route :- GetAccountSettings
    , putAccountSettings :: route :- PutAccountSettings
    , deleteAccount :: route :- DeleteAccount
    } deriving (Generic)

-- | The order of the items is not guaranteed to be the same for every call.
type GetItemUUIDs
     = ProtectAPI :> "tickler" :> "uuids" :> Get '[ JSON] [ItemUUID]

-- | The order of the items is not guaranteed to be the same for every call.
type GetItems
     = ProtectAPI :> "tickler" :> "items" :> QueryParam "filter" ItemFilter :> Get '[ JSON] [ItemInfo TypedItem]

instance ToParam (QueryParam "filter" ItemFilter) where
    toParam Proxy =
        DocQueryParam
            "filter"
            (map show [minBound .. maxBound :: ItemFilter])
            "Optionally specify a filter on the items"
            Normal

type PostAddItem
     = ProtectAPI :> "tickler" :> "item" :> ReqBody '[ JSON] AddItem :> Post '[ JSON] ItemUUID

type GetItem
     = ProtectAPI :> "tickler" :> "item" :> Capture "id" ItemUUID :> Get '[ JSON] (ItemInfo TypedItem)

type DeleteItem
     = ProtectAPI :> "item" :> Capture "id" ItemUUID :> Delete '[ JSON] NoContent

type PostSync
     = ProtectAPI :> "sync" :> ReqBody '[ JSON] SyncRequest :> Post '[ JSON] SyncResponse

type GetTriggers = ProtectAPI :> "trigger" :> Get '[ JSON] [TriggerInfo]

type PostAddIntrayTrigger
     = ProtectAPI :> "trigger" :> "intray" :> ReqBody '[ JSON] AddIntrayTrigger :> Post '[ JSON] TriggerUUID

type PostAddEmailTrigger
     = ProtectAPI :> "trigger" :> "email" :> ReqBody '[ JSON] AddEmailTrigger :> Post '[ JSON] TriggerUUID

type GetAccountInfo = ProtectAPI :> "account" :> Get '[ JSON] AccountInfo

type GetAccountSettings
     = ProtectAPI :> "account" :> "settings" :> Get '[ JSON] AccountSettings

type PutAccountSettings
     = ProtectAPI :> "account" :> "settings" :> ReqBody '[ JSON] AccountSettings :> Put '[ JSON] NoContent

type DeleteAccount = ProtectAPI :> "account" :> Delete '[ JSON] NoContent
