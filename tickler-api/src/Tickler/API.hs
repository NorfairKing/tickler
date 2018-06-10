{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Tickler.API
    ( TicklerAPI
    , ticklerAPI
    , TicklerSite(..)
    , TicklerOpenAPI
    , ticklerOpenAPI
    , TicklerOpenSite(..)
    , TicklerProtectedAPI
    , TicklerProtectedSite(..)
    , TicklerPublicAPI
    , TicklerPublicSite(..)
    , TicklerAdminAPI
    , TicklerAdminSite(..)
    , AuthCookie(..)
    , ItemFilter(..)
    , ItemType(..)
    , TypedItem(..)
    , textTypedItem
    , TypedItemCase(..)
    , typedItemCase
    , ItemInfo(..)
    , AddItem(..)
    , Added(..)
    , Synced(..)
    , SyncRequest(..)
    , SyncResponse(..)
    , TriggerUUID
    , TriggerType(..)
    , TriggerInfo(..)
    , decodeTriggerInfo
    , TypedTriggerInfo(..)
    , decodeTypedTriggerInfo
    , IntrayTriggerInfo(..)
    , EmailTriggerInfo(..)
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
    , GetAccountSettings
    , PutAccountSettings
    , Registration(..)
    , PostRegister
    , LoginForm(..)
    , PostLogin
    , GetLoopersStatus
    , LoopersStatus(..)
    , LooperStatus(..)
    , GetDocs
    , GetDocsResponse(..)
    , AdminStats(..)
    , AdminGetStats
    , AdminDeleteAccount
    , AdminGetAccounts
    , HashedPassword
    , passwordHash
    , validatePassword
    , ItemUUID
    , AccountUUID
    , Username
    , parseUsername
    , parseUsernameWithError
    , usernameText
    , module Data.UUID.Typed
    ) where

import Import

import Data.UUID.Typed

import Web.Cookie

import Servant.API
import Servant.Auth.Docs ()
import Servant.Auth.Server.SetCookieOrphan ()
import Servant.Generic
import Servant.HTML.Blaze

import Tickler.Data

import Tickler.API.Admin
import Tickler.API.Protected
import Tickler.API.Types

ticklerAPI :: Proxy TicklerAPI
ticklerAPI = Proxy

type TicklerAPI = ToServant (TicklerSite AsApi)

data TicklerSite route = TicklerSite
    { openSite :: route :- ToServant (TicklerOpenSite AsApi)
    , adminSite :: route :- "admin" :> ToServant (TicklerAdminSite AsApi)
    } deriving (Generic)

ticklerOpenAPI :: Proxy TicklerOpenAPI
ticklerOpenAPI = Proxy

type TicklerOpenAPI = ToServant (TicklerOpenSite AsApi)

data TicklerOpenSite route = TicklerOpenSite
    { protectedSite :: route :- ToServant (TicklerProtectedSite AsApi)
    , publicSite :: route :- ToServant (TicklerPublicSite AsApi)
    } deriving (Generic)

type TicklerPublicAPI = ToServant (TicklerPublicSite AsApi)

data TicklerPublicSite route = TicklerPublicSite
    { postRegister :: route :- PostRegister
    , postLogin :: route :- PostLogin
    , getLoopersStatus :: route :- GetLoopersStatus
    , getDocs :: route :- GetDocs
    } deriving (Generic)

-- | The order of the items is not guaranteed to be the same for every call.
type PostRegister
     = "item" :> ReqBody '[ JSON] Registration :> Post '[ JSON] NoContent

type PostLogin
     = "login" :> ReqBody '[ JSON] LoginForm :> PostNoContent '[ JSON] (Headers '[ Header "Set-Cookie" SetCookie, Header "Set-Cookie" SetCookie] NoContent)

type GetLoopersStatus = "loopers" :> Get '[ JSON] LoopersStatus

type GetDocs = Get '[ HTML] GetDocsResponse
