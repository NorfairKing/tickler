{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Tickler.Web.Server.Static where

import Tickler.Web.Server.Constants
import Yesod.EmbeddedStatic
import Yesod.EmbeddedStatic.Remote

mkEmbeddedStatic
  development
  "myStatic"
  [ embedFile "static/gtd_flowchart.jpg",
    embedFile "static/favicon.ico",
    embedFile "static/tom-sydney-kerckhove_tasks.svg",
    embedFile "static/tom-sydney-kerckhove_flowchart-gtd.svg",
    embedFile "static/tom-sydney-kerckhove_faq.svg",
    embedFile "static/tom-sydney-kerckhove_github-icon.svg",
    embedFile "static/tom-sydney-kerckhove_linkedin-icon.svg",
    embedFile "static/tom-sydney-kerckhove_tiktok-icon.svg",
    embedFile "static/tom-sydney-kerckhove_twitter-icon.svg",
    embedFile "static/tom-sydney-kerckhove_youtube-icon.svg",
    embedFile "static/tom-sydney-kerckhove_logo-tickler.svg",
    embedRemoteFile
      "static/jquery/jquery.min.js"
      "https://code.jquery.com/jquery-3.1.1.min.js",
    embedRemoteFile
      "static/bulma/bulma.min.css"
      "https://cdn.jsdelivr.net/npm/bulma@0.9.2/css/bulma.min.css",
    embedRemoteFile
      "static/bulma/bulma-tooltip.min.css"
      "https://cdn.jsdelivr.net/npm/bulma-tooltip@3.0.2/dist/css/bulma-tooltip.min.css"
  ]
