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
  , PostEmailTriggerVerify
  , EmailVerificationKey(..)
  , emailVerificationKeyText
  , parseEmailVerificationKeyText
  , PostEmailTriggerResendVerificationEmail
  , AccountInfo(..)
  , AccountSettings(..)
  , GetAccountSettings
  , PutAccountSettings
  , Registration(..)
  , PostRegister
  , LoginForm(..)
  , PostLogin
  , GetLoopersStatus
  , LoopersInfo(..)
  , LooperInfo(..)
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
import Servant.API.Generic
import Servant.Auth.Docs ()
import Servant.HTML.Blaze

import Tickler.Data

import Tickler.API.Admin
import Tickler.API.Protected
import Tickler.API.Types

ticklerAPI :: Proxy TicklerAPI
ticklerAPI = Proxy

type TicklerAPI = ToServantApi TicklerSite

data TicklerSite route =
  TicklerSite
    { openSite :: route :- ToServantApi TicklerOpenSite
    , adminSite :: route :- "admin" :> ToServantApi TicklerAdminSite
    }
  deriving (Generic)

ticklerOpenAPI :: Proxy TicklerOpenAPI
ticklerOpenAPI = Proxy

type TicklerOpenAPI = ToServantApi TicklerOpenSite

data TicklerOpenSite route =
  TicklerOpenSite
    { protectedSite :: route :- ToServantApi TicklerProtectedSite
    , publicSite :: route :- ToServantApi TicklerPublicSite
    }
  deriving (Generic)

type TicklerPublicAPI = ToServantApi TicklerPublicSite

data TicklerPublicSite route =
  TicklerPublicSite
    { postRegister :: route :- PostRegister
    , postLogin :: route :- PostLogin
    , getLoopersStatus :: route :- GetLoopersStatus
    , getDocs :: route :- GetDocs
    }
  deriving (Generic)

type PostRegister = "register" :> ReqBody '[ JSON] Registration :> Post '[ JSON] NoContent

type PostLogin
   = "login" :> ReqBody '[ JSON] LoginForm :> PostNoContent '[ JSON] (Headers '[ Header "Set-Cookie" SetCookie, Header "Set-Cookie" SetCookie] NoContent)

type GetLoopersStatus = "loopers" :> Get '[ JSON] LoopersInfo

type GetDocs = Get '[ HTML] GetDocsResponse
