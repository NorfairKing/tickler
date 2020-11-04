{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Tickler.Server.Handler.Public.GetDocs
  ( serveGetDocs,
  )
where

import Data.FileEmbed
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Lazy as LT
import Import
import Servant.Docs as Docs
import qualified Text.Markdown as Markdown
import Tickler.API
import Tickler.Server.Types

serveGetDocs :: TicklerHandler GetDocsResponse
serveGetDocs = pure ticklerHtmlResponse

ticklerHtmlResponse :: GetDocsResponse
ticklerHtmlResponse =
  GetDocsResponse
    $ Markdown.markdown Markdown.defaultMarkdownSettings {Markdown.msXssProtect = False}
    $ LT.fromStrict ticklerDocs

ticklerDocs :: Text
ticklerDocs =
  T.unlines
    . map
      ( \t ->
          if T.isPrefixOf "```" (T.stripStart t)
            then T.stripStart t
            else t
      )
    . T.lines
    . T.pack
    $ Docs.markdown
    $ Docs.docsWithIntros [intr]
    $ Docs.pretty ticklerOpenAPI
  where
    intr =
      Docs.DocIntro
        "Tickler API"
        [ unlines
            ["<style>", T.unpack $ TE.decodeUtf8 $(embedFile "res/style/docs.css"), "</style>"]
        ]
