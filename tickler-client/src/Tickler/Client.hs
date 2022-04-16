{-# LANGUAGE DataKinds #-}

module Tickler.Client
  ( module Tickler.Client,
    module Tickler.API,
    module Data.UUID.Typed,
    NoContent (..),
    Token,
    module Servant.Client,
  )
where

import qualified Data.UUID.Typed
import Import
import Servant.API
import Servant.API.Flatten
import Servant.Auth.Client
import Servant.Client
import Tickler.API

clientGetItems :: Token -> ClientM [ItemInfo TypedItem]
clientPostAddItem :: Token -> AddItem -> ClientM ItemUUID
clientGetItem :: Token -> ItemUUID -> ClientM (ItemInfo TypedItem)
clientPostItem :: Token -> ItemUUID -> Tickle TypedItem -> ClientM NoContent
clientDeleteItem :: Token -> ItemUUID -> ClientM NoContent
clientGetTriggers :: Token -> ClientM [TriggerInfo TypedTriggerInfo]
clientGetTrigger :: Token -> TriggerUUID -> ClientM (TriggerInfo TypedTriggerInfo)
clientPostAddIntrayTrigger :: Token -> AddIntrayTrigger -> ClientM (Either Text TriggerUUID)
clientPostAddEmailTrigger :: Token -> AddEmailTrigger -> ClientM TriggerUUID
clientPostEmailTriggerVerify :: Token -> TriggerUUID -> EmailVerificationKey -> ClientM NoContent
clientPostEmailTriggerResendVerificationEmail :: Token -> TriggerUUID -> ClientM NoContent
clientDeleteTrigger :: Token -> TriggerUUID -> ClientM NoContent
clientGetAccountInfo :: Token -> ClientM AccountInfo
clientGetAccountSettings :: Token -> ClientM AccountSettings
clientPostChangePassphrase :: Token -> ChangePassphrase -> ClientM NoContent
clientPutAccountSettings :: Token -> AccountSettings -> ClientM NoContent
clientDeleteAccount :: Token -> ClientM NoContent
clientPostRegister :: Registration -> ClientM NoContent
clientPostLogin :: LoginForm -> ClientM (Headers '[Header "Set-Cookie" Text] NoContent)
clientGetPricing :: ClientM (Maybe Pricing)
clientAdminGetStats :: Token -> ClientM AdminStats
clientAdminDeleteAccount :: Token -> AccountUUID -> ClientM NoContent
clientAdminGetAccounts :: Token -> ClientM [AccountInfo]
clientGetItems :<|> clientPostAddItem :<|> clientGetItem :<|> clientPostItem :<|> clientDeleteItem :<|> clientGetTriggers :<|> clientGetTrigger :<|> clientPostAddIntrayTrigger :<|> clientPostAddEmailTrigger :<|> clientPostEmailTriggerVerify :<|> clientPostEmailTriggerResendVerificationEmail :<|> clientDeleteTrigger :<|> clientGetAccountInfo :<|> clientGetAccountSettings :<|> clientPostChangePassphrase :<|> clientPutAccountSettings :<|> clientDeleteAccount :<|> clientPostRegister :<|> clientPostLogin :<|> clientGetPricing :<|> clientAdminGetStats :<|> clientAdminDeleteAccount :<|> clientAdminGetAccounts =
  client (flatten ticklerAPI)
