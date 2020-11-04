module Tickler.Web.Server.Handler.EmailTriggerVerify
  ( getEmailTriggerVerifyR,
  )
where

import Import
import Tickler.API
import Tickler.Client
import Tickler.Web.Server.Foundation
import Yesod

getEmailTriggerVerifyR :: TriggerUUID -> EmailVerificationKey -> Handler Html
getEmailTriggerVerifyR tuuid evk =
  withLogin $ \t -> do
    NoContent <- runClientOrErr $ clientPostEmailTriggerVerify t tuuid evk
    redirect TriggersR
