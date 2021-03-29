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
    embedRemoteFileAt
      "static/semantic/themes/default/assets/fonts/icons.ttf"
      "https://cdnjs.cloudflare.com/ajax/libs/semantic-ui/2.4.1/themes/default/assets/fonts/icons.ttf",
    embedRemoteFileAt
      "static/semantic/themes/default/assets/fonts/icons.woff"
      "https://cdnjs.cloudflare.com/ajax/libs/semantic-ui/2.4.1/themes/default/assets/fonts/icons.woff",
    embedRemoteFileAt
      "static/semantic/themes/default/assets/fonts/icons.woff2"
      "https://cdnjs.cloudflare.com/ajax/libs/semantic-ui/2.4.1/themes/default/assets/fonts/icons.woff2",
    embedRemoteFileAt "static/jquery.min.js" "https://code.jquery.com/jquery-3.1.1.min.js",
    embedRemoteFileAt
      "static/semantic/semantic.min.css"
      "https://cdnjs.cloudflare.com/ajax/libs/semantic-ui/2.4.1/semantic.min.css",
    embedRemoteFileAt
      "static/semantic/semantic.min.js"
      "https://cdnjs.cloudflare.com/ajax/libs/semantic-ui/2.4.1/semantic.min.js"
  ]
