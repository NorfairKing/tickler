module Tickler.Server.Types where

import Import

import Database.Persist.Sqlite

import Data.Cache

import Servant
import Servant.Auth.Server

import Web.Stripe.Plan as Stripe

import Tickler.API
import Tickler.Server.Looper
import Tickler.Server.OptParse.Types

data TicklerServerEnv =
  TicklerServerEnv
    { envConnectionPool :: ConnectionPool
    , envCookieSettings :: CookieSettings
    , envJWTSettings :: JWTSettings
    , envAdmins :: [Username]
    , envMonetisation :: Maybe MonetisationEnv
    , envLoopersHandle :: LoopersHandle
    }

data MonetisationEnv =
  MonetisationEnv
    { monetisationEnvStripeSettings :: StripeSettings
    , monetisationEnvMaxItemsFree :: !Int
    , monetisationEnvPlanCache :: !(Cache Stripe.PlanId Stripe.Plan)
    }

type TicklerHandler = ReaderT TicklerServerEnv Handler
