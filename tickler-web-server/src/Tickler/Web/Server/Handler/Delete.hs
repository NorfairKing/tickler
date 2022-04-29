module Tickler.Web.Server.Handler.Delete
  ( postDeleteR,
  )
where

import Import
import Tickler.API
import Tickler.Client
import Tickler.Web.Server.Foundation
import Yesod

postDeleteR :: ItemUUID -> Handler Html
postDeleteR uuid =
  withLogin $ \t -> do
    void $ runClientOrErr $ clientDeleteItem t uuid
    redirect TicklesR
