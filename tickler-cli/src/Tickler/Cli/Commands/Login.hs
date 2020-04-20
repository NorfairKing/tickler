{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Tickler.Cli.Commands.Login
  ( login
  ) where

import Import

import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)
import Servant
import Web.Cookie

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
    Just (Headers NoContent (HCons sessionHeader HNil)) ->
      case sessionHeader of
        MissingHeader ->
          liftIO $ die "The server responded but the response was missing the right session header."
        UndecodableHeader _ ->
          liftIO $ die "The server responded but the response had an undecodable session header."
        Header cookieText -> do
          let cookies = parseSetCookie . encodeUtf8 <$> T.lines cookieText
              jwtCookie = find ((== "JWT-Cookie") . setCookieName) cookies
          case jwtCookie of
            Nothing -> liftIO $ die "The server responded but the response contained no JWT-Cookie."
            Just setCookie -> saveSession setCookie
