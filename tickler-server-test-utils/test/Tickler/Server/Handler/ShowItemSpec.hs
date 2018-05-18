module Tickler.Server.Handler.ShowItemSpec
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
    describe "GetShowItem" $
    it "shows no item if the tickler is empty, even if there are items in other accounts' ticklers" $ \cenv ->
        forAllValid $ \t ->
            withValidNewUser cenv $ \t1 ->
                withValidNewUser cenv $ \t2 -> do
                    mr <-
                        runClientOrError cenv $ do
                            void $ clientPostAddItem t1 t
                            clientGetShowItem t2
                    mr `shouldBe` Nothing
