{-# LANGUAGE CPP #-}

module Tickler.Web.Server.Constants where

import Import

development :: Bool
#ifdef DEVELOPMENT
development = True
#else
development = False
#endif
