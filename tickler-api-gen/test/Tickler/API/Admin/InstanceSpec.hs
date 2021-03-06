{-# LANGUAGE TypeApplications #-}

module Tickler.API.Admin.InstanceSpec
  ( spec,
  )
where

import Test.Validity.Aeson
import TestImport
import Tickler.API.Admin.Gen ()
import Tickler.API.Admin.Types

spec :: Spec
spec = do
  genValidSpec @AdminStats
  jsonSpecOnValid @AdminStats
  genValidSpec @ActiveUsers
  jsonSpecOnValid @ActiveUsers
