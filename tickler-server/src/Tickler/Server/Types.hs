module Tickler.Server.Types where

import Data.Cache
import Database.Persist.Sqlite
import Import
import Servant
import Servant.Auth.Server
import Tickler.API
import Tickler.Server.Looper
import Tickler.Server.OptParse.Types
import Web.Stripe.Plan as Stripe

data TicklerServerEnv
  = TicklerServerEnv
      { envConnectionPool :: ConnectionPool,
        envCookieSettings :: CookieSettings,
        envJWTSettings :: JWTSettings,
        envAdmins :: [Username],
        envFreeloaders :: [Username],
        envMonetisation :: Maybe MonetisationEnv,
        envLoopersHandle :: LoopersHandle
      }

data MonetisationEnv
  = MonetisationEnv
      { monetisationEnvStripeSettings :: StripeSettings,
        monetisationEnvMaxItemsFree :: !Int,
        monetisationEnvPlanCache :: !(Cache Stripe.PlanId Stripe.Plan)
      }

type TicklerHandler = ReaderT TicklerServerEnv Handler
