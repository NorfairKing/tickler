{-# LANGUAGE DataKinds #-}

module Tickler.Client
  ( module Tickler.Client
  , module Tickler.Client.Store
  , module Tickler.API
  , module Data.UUID.Typed
  , NoContent(..)
  , Token
  , module Servant.Client
  ) where

import Import

import qualified Data.UUID.Typed

import Servant.API
import Servant.API.Flatten
import Servant.Auth.Client
import Servant.Auth.Server hiding (BasicAuth)
import Servant.Client

import Tickler.API

import Tickler.Client.Store

clientGetAllItems :: Token -> ClientM [ItemInfo TypedItem]
clientGetAllItems t = clientGetItems t Nothing

clientGetItemUUIDs :: Token -> ClientM [ItemUUID]
clientGetItems :: Token -> Maybe ItemFilter -> ClientM [ItemInfo TypedItem]
clientPostAddItem :: Token -> AddItem -> ClientM ItemUUID
clientGetItem :: Token -> ItemUUID -> ClientM (ItemInfo TypedItem)
clientDeleteItem :: Token -> ItemUUID -> ClientM NoContent
clientRetryTriggered :: Token -> [ItemUUID] -> ClientM NoContent
clientDeleteTriggereds :: Token -> ClientM NoContent
clientPostSync :: Token -> SyncRequest -> ClientM SyncResponse
clientGetTriggers :: Token -> ClientM [TriggerInfo TypedTriggerInfo]
clientGetTrigger :: Token -> TriggerUUID -> ClientM (TriggerInfo TypedTriggerInfo)
clientPostAddIntrayTrigger :: Token -> AddIntrayTrigger -> ClientM (Either Text TriggerUUID)
clientPostAddEmailTrigger :: Token -> AddEmailTrigger -> ClientM TriggerUUID
clientPostEmailTriggerVerify :: Token -> TriggerUUID -> EmailVerificationKey -> ClientM NoContent
clientPostEmailTriggerResendVerificationEmail :: Token -> TriggerUUID -> ClientM NoContent
clientDeleteTrigger :: Token -> TriggerUUID -> ClientM NoContent
clientGetAccountInfo :: Token -> ClientM AccountInfo
clientGetAccountSettings :: Token -> ClientM AccountSettings
clientPutAccountSettings :: Token -> AccountSettings -> ClientM NoContent
clientDeleteAccount :: Token -> ClientM NoContent
clientPostRegister :: Registration -> ClientM NoContent
clientPostLogin ::
     LoginForm
  -> ClientM (Headers '[ Header "Set-Cookie" SetCookie, Header "Set-Cookie" SetCookie] NoContent)
clientGetLoopersInfo :: ClientM LoopersInfo
clientGetDocs :: ClientM GetDocsResponse
clientAdminGetStats :: Token -> ClientM AdminStats
clientAdminDeleteAccount :: Token -> AccountUUID -> ClientM NoContent
clientAdminGetAccounts :: Token -> ClientM [AccountInfo]
clientGetItemUUIDs :<|> clientGetItems :<|> clientPostAddItem :<|> clientGetItem :<|> clientDeleteItem :<|> clientRetryTriggered :<|> clientDeleteTriggereds :<|> clientPostSync :<|> clientGetTriggers :<|> clientGetTrigger :<|> clientPostAddIntrayTrigger :<|> clientPostAddEmailTrigger :<|> clientPostEmailTriggerVerify :<|> clientPostEmailTriggerResendVerificationEmail :<|> clientDeleteTrigger :<|> clientGetAccountInfo :<|> clientGetAccountSettings :<|> clientPutAccountSettings :<|> clientDeleteAccount :<|> clientPostRegister :<|> clientPostLogin :<|> clientGetLoopersInfo :<|> clientGetDocs :<|> clientAdminGetStats :<|> clientAdminDeleteAccount :<|> clientAdminGetAccounts =
  client (flatten ticklerAPI)
