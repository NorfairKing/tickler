{-# LANGUAGE DataKinds #-}

module Tickler.Client
    ( clientGetAllItems
    , clientGetItemUUIDs
    , clientGetItems
    , clientPostAddItem
    , clientGetItem
    , clientDeleteItem
    , clientPostSync
    , clientGetTriggers
    , clientGetTrigger
    , clientPostAddIntrayTrigger
    , clientPostAddEmailTrigger
    , clientDeleteTrigger
    , clientPostRegister
    , clientPostLogin
    , clientGetLoopersStatus
    , clientGetDocs
    , clientGetAccountInfo
    , clientGetAccountSettings
    , clientPutAccountSettings
    , clientDeleteAccount
    , clientAdminGetStats
    , clientAdminDeleteAccount
    , clientAdminGetAccounts
    , managerSetsFor
    , ItemFilter(..)
    , ItemType(..)
    , TypedItem(..)
    , textTypedItem
    , TypedItemCase(..)
    , typedItemCase
    , ItemInfo(..)
    , AddItem
    , Added(..)
    , Synced(..)
    , SyncRequest(..)
    , SyncResponse(..)
    , TriggerInfo(..)
    , TriggerUUID
    , GetTriggers
    , AddIntrayTrigger(..)
    , PostAddIntrayTrigger
    , EmailAddress
    , normalizeEmail
    , unsafeEmailAddress
    , emailValidateFromText
    , emailValidateFromString
    , emailAddressFromText
    , emailAddressFromString
    , emailAddressText
    , emailAddressByteString
    , domainPart
    , localPart
    , AddEmailTrigger(..)
    , PostAddEmailTrigger
    , AccountInfo(..)
    , AccountSettings(..)
    , Registration(..)
    , LoginForm(..)
    , GetDocsResponse(..)
    , AdminStats(..)
    , ItemUUID
    , AccountUUID
    , Username
    , parseUsername
    , parseUsernameWithError
    , usernameText
    , NoContent(..)
    , Token
    , module Data.UUID.Typed
    ) where

import Import

import qualified Data.UUID.Typed
import qualified Network.HTTP.Client as Http
import qualified Network.HTTP.Client.TLS as Http

import Servant.API
import Servant.API.Flatten
import Servant.Auth.Client
import Servant.Auth.Server
import Servant.Auth.Server.SetCookieOrphan ()
import Servant.Client

import Tickler.API

clientGetAllItems :: Token -> ClientM [ItemInfo TypedItem]
clientGetAllItems t = clientGetItems t Nothing

clientGetItemUUIDs :: Token -> ClientM [ItemUUID]
clientGetItems :: Token -> Maybe ItemFilter -> ClientM [ItemInfo TypedItem]
clientPostAddItem :: Token -> AddItem -> ClientM ItemUUID
clientGetItem :: Token -> ItemUUID -> ClientM (ItemInfo TypedItem)
clientDeleteItem :: Token -> ItemUUID -> ClientM NoContent
clientPostSync ::
       Token
    -> (SyncRequest ItemUUID TypedItem)
    -> ClientM (SyncResponse ItemUUID TypedItem)
clientGetTriggers :: Token -> ClientM [TriggerInfo TypedTriggerInfo]
clientGetTrigger ::
       Token -> TriggerUUID -> ClientM (TriggerInfo TypedTriggerInfo)
clientPostAddIntrayTrigger ::
       Token -> AddIntrayTrigger -> ClientM (Either Text TriggerUUID)
clientPostAddEmailTrigger :: Token -> AddEmailTrigger -> ClientM TriggerUUID
clientDeleteTrigger :: Token -> TriggerUUID -> ClientM NoContent
clientGetAccountInfo :: Token -> ClientM AccountInfo
clientGetAccountSettings :: Token -> ClientM AccountSettings
clientPutAccountSettings :: Token -> AccountSettings -> ClientM NoContent
clientDeleteAccount :: Token -> ClientM NoContent
clientPostRegister :: Registration -> ClientM NoContent
clientPostLogin ::
       LoginForm
    -> ClientM (Headers '[ Header "Set-Cookie" SetCookie, Header "Set-Cookie" SetCookie] NoContent)
clientGetLoopersStatus :: ClientM LoopersStatus
clientGetDocs :: ClientM GetDocsResponse
clientAdminGetStats :: Token -> ClientM AdminStats
clientAdminDeleteAccount :: Token -> AccountUUID -> ClientM NoContent
clientAdminGetAccounts :: Token -> ClientM [AccountInfo]
clientGetItemUUIDs :<|> clientGetItems :<|> clientPostAddItem :<|> clientGetItem :<|> clientDeleteItem :<|> clientPostSync :<|> clientGetTriggers :<|> clientGetTrigger :<|> clientPostAddIntrayTrigger :<|> clientPostAddEmailTrigger :<|> clientDeleteTrigger :<|> clientGetAccountInfo :<|> clientGetAccountSettings :<|> clientPutAccountSettings :<|> clientDeleteAccount :<|> clientPostRegister :<|> clientPostLogin :<|> clientGetLoopersStatus :<|> clientGetDocs :<|> clientAdminGetStats :<|> clientAdminDeleteAccount :<|> clientAdminGetAccounts =
    client (flatten ticklerAPI)

managerSetsFor :: BaseUrl -> Http.ManagerSettings
managerSetsFor burl =
    case baseUrlScheme burl of
        Http -> Http.defaultManagerSettings
        Https -> Http.tlsManagerSettings
