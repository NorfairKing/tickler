module Tickler.Server.Handler.Public
    ( TicklerPublicSite
    , ticklerPublicServer
    ) where

import Servant.Generic

import Tickler.API

import Tickler.Server.Types

import Tickler.Server.Handler.Public.GetDocs
import Tickler.Server.Handler.Public.PostLogin
import Tickler.Server.Handler.Public.PostRegister

ticklerPublicServer :: TicklerPublicSite (AsServerT TicklerHandler)
ticklerPublicServer =
    TicklerPublicSite
    { postRegister = servePostRegister
    , postLogin = servePostLogin
    , getDocs = serveGetDocs
    }
