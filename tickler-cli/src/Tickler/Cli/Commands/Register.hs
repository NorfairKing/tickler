{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}

module Tickler.Cli.Commands.Register
  ( register,
  )
where

import Import
import Tickler.API
import Tickler.Cli.Client
import Tickler.Cli.OptParse
import Tickler.Cli.Prompt
import Tickler.Client

register :: RegisterSettings -> CliM ()
register RegisterSettings {..} = do
  registration <-
    Registration <$> promptUsername registerSetUsername <*> promptPassword registerSetPassword
  void $ runSingleClientOrErr $ clientPostRegister registration
