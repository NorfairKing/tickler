module Tickler.Web.Server.Handler.DeleteTriggereds
  ( postDeleteTriggeredsR,
  )
where

import Import
import Tickler.Client
import Tickler.Web.Server.Foundation
import Yesod

postDeleteTriggeredsR :: Handler Html
postDeleteTriggeredsR =
  withLogin $ \t -> do
    NoContent <- runClientOrErr $ clientDeleteTriggereds t
    redirect TriggeredsR
