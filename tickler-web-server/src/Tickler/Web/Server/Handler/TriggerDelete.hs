module Tickler.Web.Server.Handler.TriggerDelete (postTriggerDeleteR) where

import Import
import Tickler.API
import Tickler.Client
import Tickler.Web.Server.Foundation
import Yesod

postTriggerDeleteR :: TriggerUUID -> Handler Html
postTriggerDeleteR uuid =
  withLogin $ \t -> do
    NoContent <- runClientOrErr $ clientDeleteTrigger t uuid
    redirect TriggersR
