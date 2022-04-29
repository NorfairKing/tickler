module Tickler.Web.Server.Handler.EmailTriggerResend
  ( postTriggerEmailResendR,
  )
where

import Import
import Tickler.API
import Tickler.Client
import Tickler.Web.Server.Foundation
import Yesod

postTriggerEmailResendR :: TriggerUUID -> Handler Html
postTriggerEmailResendR tuuid =
  withLogin $ \t -> do
    NoContent <- runClientOrErr $ clientPostEmailTriggerResendVerificationEmail t tuuid
    redirect TriggersR
