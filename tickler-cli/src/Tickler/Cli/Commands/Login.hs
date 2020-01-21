{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Tickler.Cli.Commands.Login
  ( login
  ) where

import Import

import Servant
import Servant.Auth.Server.SetCookieOrphan ()

import Tickler.API

import Tickler.Client

import Tickler.Cli.Client
import Tickler.Cli.OptParse
import Tickler.Cli.Prompt
import Tickler.Cli.Session

login :: LoginSettings -> CliM ()
login LoginSettings {..} = do
  sets <- ask
  mRes <-
    runSingleClientOrErr $ do
      loginForm <-
        liftIO $
        runReaderT
          (LoginForm <$> promptUsername loginSetUsername <*> promptPassword loginSetPassword)
          sets
      clientPostLogin loginForm
  case mRes of
    Nothing -> liftIO $ die "No server configured."
    Just (Headers NoContent (HCons _ (HCons sessionHeader HNil))) ->
      case sessionHeader of
        MissingHeader -> liftIO $ die "Missing header" -- TODO handle nicely
        UndecodableHeader _ -> liftIO $ die "Undecodable header" -- TODO handle nicely
        Header setCookie -> saveSession setCookie
