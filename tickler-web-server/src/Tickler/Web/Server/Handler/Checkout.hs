module Tickler.Web.Server.Handler.Checkout
  ( getCheckoutSuccessR,
    getCheckoutCanceledR,
  )
where

import Tickler.Web.Server.Foundation
import Yesod

getCheckoutSuccessR :: Handler Html
getCheckoutSuccessR = redirect AccountR

getCheckoutCanceledR :: Handler Html
getCheckoutCanceledR = redirect AccountR
