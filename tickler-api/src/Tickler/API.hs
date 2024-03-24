{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Tickler.API
  ( module Tickler.API,
    module Tickler.API.Account,
    module Tickler.API.Admin,
    module Tickler.API.Protected,
    module Tickler.Data,
    module Data.UUID.Typed,
  )
where

import Autodocodec
import Data.Aeson (FromJSON, ToJSON)
import qualified Data.Aeson as JSON
import Data.UUID.Typed
import Import
import Intray.API ()
import Servant.API
import Tickler.API.Account
import Tickler.API.Admin
import Tickler.API.Protected
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
    getPricing :: route :- GetPricing,
    postStripeHook :: !(route :- PostStripeHook)
  }
  deriving (Generic)

type PostRegister =
  "register"
    :> ReqBody '[JSON] Registration
    :> Verb 'POST 204 '[JSON] NoContent

type PostLogin =
  "login"
    :> ReqBody '[JSON] LoginForm
    :> Verb 'POST 204 '[JSON] (Headers '[Header "Set-Cookie" Text] NoContent)

type GetPricing = "pricing" :> Get '[JSON] (Maybe Pricing)

data Registration = Registration
  { registrationUsername :: Username,
    registrationPassword :: Text
  }
  deriving stock (Show, Eq, Generic)
  deriving (FromJSON, ToJSON) via (Autodocodec Registration)

instance Validity Registration

instance HasCodec Registration where
  codec =
    object "Registration"
      $ Registration
      <$> requiredField "username" "username"
      .= registrationUsername
      <*> requiredField "password" "password"
      .= registrationPassword

data LoginForm = LoginForm
  { loginFormUsername :: Username,
    loginFormPassword :: Text
  }
  deriving stock (Show, Eq, Generic)
  deriving (FromJSON, ToJSON) via (Autodocodec LoginForm)

instance Validity LoginForm

instance HasCodec LoginForm where
  codec =
    object "LoginForm"
      $ LoginForm
      <$> requiredField "username" "username"
      .= loginFormUsername
      <*> requiredField "password" "password"
      .= loginFormPassword

data Pricing = Pricing
  { pricingPlan :: !Text, -- Stripe plan id
    pricingPrice :: !Text,
    pricingStripePublishableKey :: !Text,
    pricingMaxItemsFree :: !Int
  }
  deriving stock (Show, Eq, Generic)
  deriving (FromJSON, ToJSON) via (Autodocodec Pricing)

instance Validity Pricing

instance HasCodec Pricing where
  codec =
    object "Pricing"
      $ Pricing
      <$> requiredField "plan" "stripe plan"
      .= pricingPlan
      <*> requiredField "price" "price"
      .= pricingPrice
      <*> requiredField "publishable-key" "publishable key"
      .= pricingStripePublishableKey
      <*> requiredField "max-items-free" "maximum number of free items"
      .= pricingMaxItemsFree

type PostStripeHook =
  "stripe"
    :> ReqBody '[JSON] JSON.Value
    :> Verb 'POST 204 '[JSON] NoContent
