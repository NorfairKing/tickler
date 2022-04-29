module Tickler.Web.Server.Handler.EmailTriggerVerify
  ( getTriggerEmailVerifyR,
  )
where

import Import
import Tickler.API
import Tickler.Client
import Tickler.Web.Server.Foundation
import Yesod

getTriggerEmailVerifyR :: TriggerUUID -> EmailVerificationKey -> Handler Html
getTriggerEmailVerifyR tuuid evk =
  withLogin $ \t -> do
    NoContent <- runClientOrErr $ clientPostEmailTriggerVerify t tuuid evk
    redirect TriggersR
