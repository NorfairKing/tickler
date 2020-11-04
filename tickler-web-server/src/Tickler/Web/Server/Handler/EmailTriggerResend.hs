module Tickler.Web.Server.Handler.EmailTriggerResend
  ( postEmailTriggerResendR,
  )
where

import Import
import Tickler.API
import Tickler.Client
import Tickler.Web.Server.Foundation
import Yesod

postEmailTriggerResendR :: TriggerUUID -> Handler Html
postEmailTriggerResendR tuuid =
  withLogin $ \t -> do
    NoContent <- runClientOrErr $ clientPostEmailTriggerResendVerificationEmail t tuuid
    redirect TriggersR
