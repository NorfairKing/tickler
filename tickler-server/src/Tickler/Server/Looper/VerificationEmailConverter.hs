{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Tickler.Server.Looper.VerificationEmailConverter
    ( runVerificationEmailConverter
    ) where

import Import

import Control.Concurrent

import Control.Monad.Logger
import Control.Monad.Trans.Resource (runResourceT)
import Data.Pool
import Data.Time
import Database.Persist.Sqlite

import Tickler.Data

import Tickler.Server.OptParse.Types

import Tickler.Server.Looper.Types

runVerificationEmailConverter :: () -> Looper ()
runVerificationEmailConverter () = do
    logInfoNS
        "TriggeredEmailConverter"
        "Starting converting VerificationEmails to Emails."
    liftIO $
        putStrLn "placeholder to convert verification emails to regular emails"
    logInfoNS
        "TriggeredEmailConverter"
        "Finished converting VerificationEmails to Emails."
