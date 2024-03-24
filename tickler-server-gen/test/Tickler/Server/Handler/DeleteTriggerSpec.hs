module Tickler.Server.Handler.DeleteTriggerSpec (spec) where

import qualified Data.Set as S
import qualified Data.Text as T
import qualified Intray.Client as Intray
import qualified Intray.Server.TestUtils as Intray
import TestImport
import Tickler.API.Gen ()
import Tickler.Client
import Tickler.Server.TestUtils

spec :: Spec
spec = do
  withTicklerServer $ do
    it "cannot delete a trigger that does not exist" $ \cenv ->
      forAllValid $ \uuid ->
        withValidNewUser cenv $ \token -> do
          errOrResult <- runClient cenv $ clientDeleteTrigger token uuid
          case errOrResult of
            Left err ->
              case err of
                FailureResponse _ resp -> responseStatusCode resp `shouldBe` notFound404
                _ -> expectationFailure $ unwords ["Unexpected error:", show err]
            Right _ -> expectationFailure "Should not have succeeded."

    it "can delete the email trigger that was just added" $ \cenv ->
      forAllValid $ \addEmailTrigger ->
        withValidNewUser cenv $ \token -> do
          uuid <- runClientOrError cenv $ clientPostEmailTrigger token addEmailTrigger
          NoContent <- runClientOrError cenv $ clientDeleteTrigger token uuid
          errOrResult <- runClient cenv $ clientGetTrigger token uuid
          case errOrResult of
            Left err ->
              case err of
                FailureResponse _ resp -> responseStatusCode resp `shouldBe` notFound404
                _ -> expectationFailure $ unwords ["Unexpected error:", show err]
            Right _ -> expectationFailure "Should not have succeeded."

  withBothTicklerAndIntrayServer
    $ it "can delete the intray trigger that was just added"
    $ \(tenv, ienv) ->
      forAllValid $ \name ->
        withValidNewUser tenv $ \ttoken ->
          Intray.withValidNewUserAndData ienv $ \un _ itoken -> do
            -- Add an intray access key that only permits adding items
            akc <-
              runClientOrError ienv
                $ Intray.clientPostAddAccessKey
                  itoken
                  Intray.AddAccessKey
                    { Intray.addAccessKeyName = name,
                      Intray.addAccessKeyPermissions = S.singleton Intray.PermitAdd
                    }
            errOrUuid <- runClientOrError tenv $ do
              clientPostIntrayTrigger
                ttoken
                AddIntrayTrigger
                  { addIntrayTriggerUrl = baseUrl ienv,
                    addIntrayTriggerUsername = un,
                    addIntrayTriggerAccessKey = Intray.accessKeyCreatedKey akc
                  }

            case errOrUuid of
              Left err -> liftIO $ expectationFailure (T.unpack err)
              Right uuid -> do
                NoContent <- runClientOrError tenv $ clientDeleteTrigger ttoken uuid
                errOrResult <- runClient tenv $ clientGetTrigger ttoken uuid
                case errOrResult of
                  Left err ->
                    case err of
                      FailureResponse _ resp -> responseStatusCode resp `shouldBe` notFound404
                      _ -> expectationFailure $ unwords ["Unexpected error:", show err]
                  Right _ -> expectationFailure "Should not have succeeded."
