{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Tickler.API
  ( module Tickler.API,
    module Tickler.API.Account.Types,
    module Tickler.API.Admin,
    module Tickler.API.Protected,
    module Tickler.API.Types,
    module Tickler.Data,
    module Data.UUID.Typed,
  )
where

import Data.UUID.Typed
import Import
import Servant.API
import Servant.API.Generic
import Tickler.API.Account.Types
import Tickler.API.Admin
import Tickler.API.Protected
import Tickler.API.Types
import Tickler.Data

ticklerAPI :: Proxy TicklerAPI
ticklerAPI = Proxy

type TicklerAPI = ToServantApi TicklerSite

data TicklerSite route = TicklerSite
  { openSite :: route :- ToServantApi TicklerOpenSite,
    adminSite :: route :- "admin" :> ToServantApi TicklerAdminSite
  }
  deriving (Generic)

ticklerOpenAPI :: Proxy TicklerOpenAPI
ticklerOpenAPI = Proxy

type TicklerOpenAPI = ToServantApi TicklerOpenSite

data TicklerOpenSite route = TicklerOpenSite
  { protectedSite :: route :- ToServantApi TicklerProtectedSite,
    publicSite :: route :- ToServantApi TicklerPublicSite
  }
  deriving (Generic)

type TicklerPublicAPI = ToServantApi TicklerPublicSite

data TicklerPublicSite route = TicklerPublicSite
  { postRegister :: route :- PostRegister,
    postLogin :: route :- PostLogin,
    getPricing :: route :- GetPricing
  }
  deriving (Generic)

type PostRegister = "register" :> ReqBody '[JSON] Registration :> Post '[JSON] NoContent

type PostLogin =
  "login" :> ReqBody '[JSON] LoginForm :> PostNoContent '[JSON] (Headers '[Header "Set-Cookie" Text] NoContent)

type GetPricing = "pricing" :> Get '[JSON] (Maybe Pricing)
