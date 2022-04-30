{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Tickler.API.Protected
  ( module Tickler.API.Protected,
    module Tickler.API.Protected.Types,
    module Tickler.API.Account.Types,
    module Tickler.API.Types,
    module Tickler.Data,
  )
where

import Import
import Servant.API
import Servant.API.Generic
import Tickler.API.Account.Types
import Tickler.API.Protected.Types
import Tickler.API.Types
import Tickler.Data

type TicklerProtectedAPI = ToServantApi TicklerProtectedSite

data TicklerProtectedSite route = TicklerProtectedSite
  { getItems :: !(route :- GetItems),
    postAddItem :: !(route :- PostItem),
    getItem :: !(route :- GetItem),
    putItem :: !(route :- PutItem),
    deleteItem :: !(route :- DeleteItem),
    getTriggers :: !(route :- GetTriggers),
    getTrigger :: !(route :- GetTrigger),
    postAddIntrayTrigger :: !(route :- PostAddIntrayTrigger),
    postAddEmailTrigger :: !(route :- PostAddEmailTrigger),
    postEmailTriggerVerify :: !(route :- PostEmailTriggerVerify),
    postEmailTriggerResendVerificationEmail :: !(route :- PostEmailTriggerResendVerificationEmail),
    deleteTrigger :: !(route :- DeleteTrigger),
    getAccountInfo :: !(route :- GetAccountInfo),
    getAccountSettings :: !(route :- GetAccountSettings),
    postChangePassphrase :: route :- PostChangePassphrase,
    putAccountSettings :: !(route :- PutAccountSettings),
    deleteAccount :: !(route :- DeleteAccount)
  }
  deriving (Generic)

-- | The order of the items is not guaranteed to be the same for every call.
type GetItems =
  ProtectAPI
    :> "item"
    :> Get '[JSON] [ItemInfo]

type PostItem =
  ProtectAPI
    :> "item"
    :> ReqBody '[JSON] Tickle
    :> Post '[JSON] ItemUUID

type GetItem =
  ProtectAPI
    :> "item"
    :> Capture "id" ItemUUID
    :> Get '[JSON] ItemInfo

type PutItem =
  ProtectAPI
    :> "item"
    :> Capture "id" ItemUUID
    :> ReqBody '[JSON] Tickle
    :> Put '[JSON] NoContent

type DeleteItem =
  ProtectAPI
    :> "item"
    :> Capture "id" ItemUUID
    :> Delete '[JSON] NoContent

type GetTriggers =
  ProtectAPI
    :> "trigger"
    :> Get '[JSON] [TriggerInfo]

type GetTrigger =
  ProtectAPI
    :> "trigger"
    :> Capture "id" TriggerUUID
    :> Get '[JSON] TriggerInfo

type PostAddIntrayTrigger =
  ProtectAPI
    :> "trigger"
    :> "intray"
    :> ReqBody '[JSON] AddIntrayTrigger
    :> Post '[JSON] (Either Text TriggerUUID)

type PostAddEmailTrigger =
  ProtectAPI
    :> "trigger"
    :> "email"
    :> ReqBody '[JSON] AddEmailTrigger
    :> Post '[JSON] TriggerUUID

type PostEmailTriggerVerify =
  ProtectAPI
    :> "trigger"
    :> "email"
    :> Capture "id" TriggerUUID
    :> "verify"
    :> Capture "key" EmailVerificationKey
    :> Post '[JSON] NoContent

type PostEmailTriggerResendVerificationEmail =
  ProtectAPI
    :> "trigger"
    :> "email"
    :> Capture "id" TriggerUUID
    :> "resend"
    :> Post '[JSON] NoContent

type DeleteTrigger =
  ProtectAPI
    :> "trigger"
    :> "delete"
    :> Capture "id" TriggerUUID
    :> Delete '[JSON] NoContent

type GetAccountInfo =
  ProtectAPI
    :> "account"
    :> Get '[JSON] AccountInfo

type GetAccountSettings =
  ProtectAPI
    :> "account"
    :> "settings"
    :> Get '[JSON] AccountSettings

type PostChangePassphrase =
  ProtectAPI
    :> ReqBody '[JSON] ChangePassphrase
    :> PostNoContent '[JSON] NoContent

type PutAccountSettings =
  ProtectAPI
    :> "account"
    :> "settings"
    :> ReqBody '[JSON] AccountSettings
    :> Put '[JSON] NoContent

type DeleteAccount =
  ProtectAPI
    :> "account"
    :> Delete '[JSON] NoContent
