module Tickler.Server.Handler.SizeSpec
    ( spec
    ) where

import TestImport

import Tickler.Client

import Tickler.Client.Gen ()
import Tickler.Data.Gen ()
import Tickler.Server.TestUtils

spec :: Spec
spec =
    withTicklerServer $
    describe "GetSize" $
    it "does not count other accounts' items" $ \cenv ->
        forAllValid $ \t ->
            withValidNewUser cenv $ \t1 ->
                withValidNewUser cenv $ \t2 -> do
                    mr <-
                        runClientOrError cenv $ do
                            void $ clientPostAddItem t1 t
                            clientGetSize t2
                    mr `shouldBe` 0
