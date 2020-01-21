module Tickler.Cli.Commands.Logout
  ( logout
  ) where

import Import

import Tickler.Cli.OptParse
import Tickler.Cli.Path

logout :: CliM ()
logout = do
  p <- sessionPath
  liftIO $ ignoringAbsence $ removeFile p
