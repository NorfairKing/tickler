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
    , Tickle(..)
    , TypedTickle
    , ItemInfo(..)
    , TypedItemInfo
    , TriggeredInfo(..)
    , TriggerAttempt(..)
    , IntrayTriggerResult(..)
    , EmailTriggerResult(..)
    , AddItem
    , Added(..)
    , Synced(..)
    , SyncRequest(..)
    , SyncResponse(..)
    , PostSync
    , TriggerUUID
    , TriggerType(..)
    , TriggerInfo(..)
    , decodeTriggerInfo
    , TypedTriggerInfo(..)
    , decodeTypedTriggerInfo
    , IntrayTriggerInfo(..)
    , EmailTriggerInfo(..)
    , GetTriggers
    , GetTrigger
    , AddIntrayTrigger(..)
    , PostAddIntrayTrigger
    , AddEmailTrigger(..)
    , PostAddEmailTrigger
    , DeleteTrigger
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
    , getTrigger :: route :- GetTrigger
    , postAddIntrayTrigger :: route :- PostAddIntrayTrigger
    , postAddEmailTrigger :: route :- PostAddEmailTrigger
    , deleteTrigger :: route :- DeleteTrigger
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
     = ProtectAPI :> "tickler" :> "item" :> "info" :> Capture "id" ItemUUID :> Get '[ JSON] (ItemInfo TypedItem)

instance ToCapture (Capture "id" ItemUUID) where
    toCapture _ = DocCapture "id" "The UUID of the item"

type DeleteItem
     = ProtectAPI :> "tickler" :> "item" :> "delete" :> Capture "id" ItemUUID :> Delete '[ JSON] NoContent

type PostSync
     = ProtectAPI :> "sync" :> ReqBody '[ JSON] SyncRequest :> Post '[ JSON] SyncResponse

type GetTriggers
     = ProtectAPI :> "trigger" :> Get '[ JSON] [TriggerInfo TypedTriggerInfo]

type GetTrigger
     = ProtectAPI :> "trigger" :> "info" :> Capture "id" TriggerUUID :> Get '[ JSON] (TriggerInfo TypedTriggerInfo)

instance ToCapture (Capture "id" TriggerUUID) where
    toCapture _ = DocCapture "id" "The UUID of the trigger"

type PostAddIntrayTrigger
     = ProtectAPI :> "trigger" :> "intray" :> ReqBody '[ JSON] AddIntrayTrigger :> Post '[ JSON] (Either Text TriggerUUID)

type PostAddEmailTrigger
     = ProtectAPI :> "trigger" :> "email" :> ReqBody '[ JSON] AddEmailTrigger :> Post '[ JSON] TriggerUUID

type DeleteTrigger
     = ProtectAPI :> "trigger" :> "delete" :> Capture "id" TriggerUUID :> Delete '[ JSON] NoContent

type GetAccountInfo = ProtectAPI :> "account" :> Get '[ JSON] AccountInfo

type GetAccountSettings
     = ProtectAPI :> "account" :> "settings" :> Get '[ JSON] AccountSettings

type PutAccountSettings
     = ProtectAPI :> "account" :> "settings" :> ReqBody '[ JSON] AccountSettings :> Put '[ JSON] NoContent

type DeleteAccount = ProtectAPI :> "account" :> Delete '[ JSON] NoContent
