{-# LANGUAGE TypeApplications #-}

module Tickler.API.Admin.InstanceSpec
  ( spec,
  )
where

import Test.Syd.Validity.Aeson
import TestImport
import Tickler.API.Admin
import Tickler.API.Admin.Gen ()

spec :: Spec
spec = do
  genValidSpec @AdminStats
  jsonSpec @AdminStats
  genValidSpec @ActiveUsers
  jsonSpec @ActiveUsers
