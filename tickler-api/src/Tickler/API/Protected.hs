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
  , RetryTriggered
  , DeleteTriggereds
  , ItemFilter(..)
  , ItemType(..)
  , TypedItem(..)
  , textTypedItem
  , TypedItemCase(..)
  , typedItemCase
  , AddedItem(..)
  , Recurrence(..)
  , everyDaysAtTime
  , everyMonthsOnDayAtTime
  , Tickle(..)
  , TypedTickle
  , ItemInfo(..)
  , TypedItemInfo
  , TriggeredInfo(..)
  , TriggerAttempt(..)
  , IntrayTriggerResult(..)
  , EmailTriggerResult(..)
  , AddItem
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
  , PostEmailTriggerVerify
  , EmailVerificationKey(..)
  , PostEmailTriggerResendVerificationEmail
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
import Servant.API.Generic
import Servant.Auth.Docs ()
import Servant.Docs

import Tickler.Data

import Tickler.API.Account.Types
import Tickler.API.Protected.Types
import Tickler.API.Types

type TicklerProtectedAPI = ToServantApi TicklerProtectedSite

data TicklerProtectedSite route =
  TicklerProtectedSite
    { getItemUUIDs :: route :- GetItemUUIDs
    , getItems :: route :- GetItems
    , postAddItem :: route :- PostAddItem
    , getItem :: route :- GetItem
    , deleteItem :: route :- DeleteItem
    , retryTriggered :: route :- RetryTriggered
    , deleteTriggereds :: route :- DeleteTriggereds
    , postSync :: route :- PostSync
    , getTriggers :: route :- GetTriggers
    , getTrigger :: route :- GetTrigger
    , postAddIntrayTrigger :: route :- PostAddIntrayTrigger
    , postAddEmailTrigger :: route :- PostAddEmailTrigger
    , postEmailTriggerVerify :: route :- PostEmailTriggerVerify
    , postEmailTriggerResendVerificationEmail :: route :- PostEmailTriggerResendVerificationEmail
    , deleteTrigger :: route :- DeleteTrigger
    , getAccountInfo :: route :- GetAccountInfo
    , getAccountSettings :: route :- GetAccountSettings
    , putAccountSettings :: route :- PutAccountSettings
    , deleteAccount :: route :- DeleteAccount
    }
  deriving (Generic)

-- | The order of the items is not guaranteed to be the same for every call.
type GetItemUUIDs = ProtectAPI :> "tickler" :> "uuids" :> Get '[ JSON] [ItemUUID]

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

type RetryTriggered
   = ProtectAPI :> "tickler" :> "item" :> "retry" :> ReqBody '[ JSON] [ItemUUID] :> Post '[ JSON] NoContent

type DeleteTriggereds
   = ProtectAPI :> "tickler" :> "item" :> "delete-triggereds" :> Post '[ JSON] NoContent

type PostSync = ProtectAPI :> "sync" :> ReqBody '[ JSON] SyncRequest :> Post '[ JSON] SyncResponse

type GetTriggers = ProtectAPI :> "trigger" :> Get '[ JSON] [TriggerInfo TypedTriggerInfo]

type GetTrigger
   = ProtectAPI :> "trigger" :> "info" :> Capture "id" TriggerUUID :> Get '[ JSON] (TriggerInfo TypedTriggerInfo)

instance ToCapture (Capture "id" TriggerUUID) where
  toCapture _ = DocCapture "id" "The UUID of the trigger"

type PostAddIntrayTrigger
   = ProtectAPI :> "trigger" :> "intray" :> ReqBody '[ JSON] AddIntrayTrigger :> Post '[ JSON] (Either Text TriggerUUID)

type PostAddEmailTrigger
   = ProtectAPI :> "trigger" :> "email" :> ReqBody '[ JSON] AddEmailTrigger :> Post '[ JSON] TriggerUUID

type PostEmailTriggerVerify
   = ProtectAPI :> "trigger" :> "email" :> "verify" :> Capture "id" TriggerUUID :> Capture "key" EmailVerificationKey :> Post '[ JSON] NoContent

instance ToCapture (Capture "key" EmailVerificationKey) where
  toCapture _ = DocCapture "key" "The verification key that was sent in the verification email"

type PostEmailTriggerResendVerificationEmail
   = ProtectAPI :> "trigger" :> "email" :> "resend" :> Capture "id" TriggerUUID :> Post '[ JSON] NoContent

type DeleteTrigger
   = ProtectAPI :> "trigger" :> "delete" :> Capture "id" TriggerUUID :> Delete '[ JSON] NoContent

type GetAccountInfo = ProtectAPI :> "account" :> Get '[ JSON] AccountInfo

type GetAccountSettings = ProtectAPI :> "account" :> "settings" :> Get '[ JSON] AccountSettings

type PutAccountSettings
   = ProtectAPI :> "account" :> "settings" :> ReqBody '[ JSON] AccountSettings :> Put '[ JSON] NoContent

type DeleteAccount = ProtectAPI :> "account" :> Delete '[ JSON] NoContent
