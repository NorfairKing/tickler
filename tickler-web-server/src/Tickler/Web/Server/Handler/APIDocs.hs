module Tickler.Web.Server.Handler.APIDocs
  ( getAPIDocsR
  ) where

import Import

import Yesod

import Tickler.API
import Tickler.Client

import Tickler.Web.Server.Foundation

getAPIDocsR :: Handler Html
getAPIDocsR = do
  GetDocsResponse html <- runClientOrErr clientGetDocs
    -- If we ever separate the API from the web server
    -- and this becomes a well-traveled route, then we may want
    -- to cache the API docs.
  pure html
