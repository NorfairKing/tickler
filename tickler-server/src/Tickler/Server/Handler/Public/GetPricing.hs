{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Tickler.Server.Handler.Public.GetPricing
  ( serveGetPricing
  ) where

import Import

import Tickler.API

import Tickler.Server.Types

serveGetPricing :: TicklerHandler (Maybe Pricing)
serveGetPricing = undefined
