module Tickler.Data.DBSpec (spec) where

import Test.Syd
import Test.Syd.Persistent.Sqlite
import Tickler.Data.DB

spec :: Spec
spec = do
  sqliteMigrationSucceedsSpec "test_resources/migration.sql" serverAutoMigration
