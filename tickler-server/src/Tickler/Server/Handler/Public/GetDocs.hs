{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Tickler.Server.Handler.Public.GetDocs
    ( serveGetDocs
    ) where

import Import

import Data.FileEmbed
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Text.Pandoc as Pandoc

import Servant hiding (BadPassword, NoSuchUser)
import Servant.Auth.Server as Auth
import Servant.Auth.Server.SetCookieOrphan ()
import Servant.Docs as Docs

import Tickler.API

import Tickler.Server.Types

serveGetDocs :: TicklerHandler GetDocsResponse
serveGetDocs =
    case ticklerHtmlResponse of
        Left _ ->
            throwError
                err500
                {errBody = "Failed to convert the docs from Markdown to HTML."}
        Right bs -> pure bs

ticklerHtmlResponse :: Either String GetDocsResponse
ticklerHtmlResponse =
    left show $
    Pandoc.runPure $ do
        md <- Pandoc.readMarkdown def ticklerDocs
        html <- Pandoc.writeHtml5 def md
        pure $ GetDocsResponse html

ticklerDocs :: Text
ticklerDocs =
    T.pack $
    Docs.markdown $ Docs.docsWithIntros [intr] $ Docs.pretty ticklerOpenAPI
  where
    intr =
        Docs.DocIntro
            "Tickler API"
            [ unlines
                  [ "<style>"
                  , T.unpack $ TE.decodeUtf8 $(embedFile "res/style/docs.css")
                  , "</style>"
                  ]
            ]
