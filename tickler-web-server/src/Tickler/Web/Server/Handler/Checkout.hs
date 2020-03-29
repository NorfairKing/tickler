module Tickler.Web.Server.Handler.Checkout
  ( getCheckoutSuccessR
  , getCheckoutCanceledR
  ) where

import Yesod

import Tickler.Web.Server.Foundation

getCheckoutSuccessR :: Handler Html
getCheckoutSuccessR = redirect AccountR

getCheckoutCanceledR :: Handler Html
getCheckoutCanceledR = redirect AccountR
