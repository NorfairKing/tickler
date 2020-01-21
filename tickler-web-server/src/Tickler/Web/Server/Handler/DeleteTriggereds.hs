module Tickler.Web.Server.Handler.DeleteTriggereds
  ( postDeleteTriggeredsR
  ) where

import Import

import Yesod

import Tickler.Client

import Tickler.Web.Server.Foundation

postDeleteTriggeredsR :: Handler Html
postDeleteTriggeredsR =
  withLogin $ \t -> do
    NoContent <- runClientOrErr $ clientDeleteTriggereds t
    redirect TriggeredsR
