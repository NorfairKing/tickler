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

import Data.Aeson as JSON
import Data.Time
import qualified Data.UUID.Typed
import Import
import Servant.API
import Servant.API.Flatten
import Servant.Auth.Client
import Servant.Client
import Tickler.API

clientGetItems :: Token -> ClientM [ItemInfo]
clientPostItem :: Token -> Tickle -> ClientM ItemUUID
clientGetItem :: Token -> ItemUUID -> ClientM ItemInfo
clientPutItem :: Token -> ItemUUID -> Tickle -> ClientM NoContent
clientDeleteItem :: Token -> ItemUUID -> ClientM NoContent
clientGetTriggers :: Token -> ClientM [TriggerInfo]
clientGetTrigger :: Token -> TriggerUUID -> ClientM TriggerInfo
clientPostIntrayTrigger :: Token -> AddIntrayTrigger -> ClientM (Either Text TriggerUUID)
clientPostEmailTrigger :: Token -> AddEmailTrigger -> ClientM TriggerUUID
clientPostEmailTriggerVerify :: Token -> TriggerUUID -> EmailVerificationKey -> ClientM NoContent
clientPostEmailTriggerResendVerificationEmail :: Token -> TriggerUUID -> ClientM NoContent
clientDeleteTrigger :: Token -> TriggerUUID -> ClientM NoContent
clientGetAccountInfo :: Token -> ClientM AccountInfo
clientGetAccountSettings :: Token -> ClientM AccountSettings
clientPostChangePassphrase :: Token -> ChangePassphrase -> ClientM NoContent
clientPutAccountSettings :: Token -> AccountSettings -> ClientM NoContent
clientDeleteAccount :: Token -> ClientM NoContent
clientPostInitiateStripeCheckoutSession :: Token -> InitiateStripeCheckoutSession -> ClientM InitiatedCheckoutSession
clientPostRegister :: Registration -> ClientM NoContent
clientPostLogin :: LoginForm -> ClientM (Headers '[Header "Set-Cookie" Text] NoContent)
clientGetPricing :: ClientM (Maybe Pricing)
clientPostStripeHook :: JSON.Value -> ClientM NoContent
clientAdminGetStats :: Token -> ClientM AdminStats
clientAdminDeleteAccount :: Token -> Username -> ClientM NoContent
clientAdminGetAccount :: Token -> Username -> ClientM AccountInfo
clientAdminGetAccounts :: Token -> ClientM [AccountInfo]
clientAdminPutAccountSubscription :: Token -> Username -> UTCTime -> ClientM NoContent
clientGetItems
  :<|> clientPostItem
  :<|> clientGetItem
  :<|> clientPutItem
  :<|> clientDeleteItem
  :<|> clientGetTriggers
  :<|> clientGetTrigger
  :<|> clientPostIntrayTrigger
  :<|> clientPostEmailTrigger
  :<|> clientPostEmailTriggerVerify
  :<|> clientPostEmailTriggerResendVerificationEmail
  :<|> clientDeleteTrigger
  :<|> clientGetAccountInfo
  :<|> clientGetAccountSettings
  :<|> clientPostChangePassphrase
  :<|> clientPutAccountSettings
  :<|> clientDeleteAccount
  :<|> clientPostInitiateStripeCheckoutSession
  :<|> clientPostRegister
  :<|> clientPostLogin
  :<|> clientGetPricing
  :<|> clientPostStripeHook
  :<|> clientAdminGetStats
  :<|> clientAdminDeleteAccount
  :<|> clientAdminGetAccount
  :<|> clientAdminGetAccounts
  :<|> clientAdminPutAccountSubscription =
    client (flatten ticklerAPI)
