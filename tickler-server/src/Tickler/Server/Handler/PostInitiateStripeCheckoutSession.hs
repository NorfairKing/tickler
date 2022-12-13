{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Tickler.Server.Handler.PostInitiateStripeCheckoutSession
  ( servePostInitiateStripeCheckoutSession,
    mkPostCheckoutSessionsRequestBodyForUser,
    mkPostCustomersRequestBodyForUser,
    metadata,
  )
where

import Data.Aeson as JSON
import Data.Aeson.Encode.Pretty as JSON
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString.Lazy as LB
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Version
import Database.Persist
import Import
import Network.HTTP.Client as HTTP
import Paths_tickler_server
import Servant
import StripeClient as Stripe
import Tickler.API
import Tickler.Server.Handler.Utils
import Tickler.Server.OptParse.Types
import Tickler.Server.Types

servePostInitiateStripeCheckoutSession ::
  AuthCookie ->
  InitiateStripeCheckoutSession ->
  TicklerHandler InitiatedCheckoutSession
servePostInitiateStripeCheckoutSession AuthCookie {..} iscs = do
  mMonetisationSettings <- asks envMonetisation
  case mMonetisationSettings of
    Nothing -> throwError err404
    Just MonetisationSettings {..} -> do
      let StripeSettings {..} = monetisationSetStripeSettings
      let config =
            Stripe.defaultConfiguration
              { configSecurityScheme = bearerAuthenticationSecurityScheme stripeSetSecretKey
              }
      mAccount <- runDB $ getBy $ UniqueUserIdentifier authCookieUserUUID
      case mAccount of
        Nothing -> throwError err404 {errBody = "User not found."}
        Just (Entity _ user@User {..}) -> do
          -- Get or create the stripe customer
          customerId <- getOrCreateCustomerId config user

          -- Make the request to create a checkout session
          let request = mkPostCheckoutSessionsRequestBodyForUser iscs userUsername customerId stripeSetPlan

          -- Actually perform the request
          resp <- liftIO $ runWithConfiguration config $ postCheckoutSessions request :: TicklerHandler (HTTP.Response PostCheckoutSessionsResponse)
          case responseBody resp of
            PostCheckoutSessionsResponseError err -> throwError err500 {errBody = LB.fromStrict $ TE.encodeUtf8 $ "Something went wrong while parsing stripe's response:" <> T.pack err}
            PostCheckoutSessionsResponseDefault err -> throwError err500 {errBody = "Error while calling stripe:\n" <> JSON.encodePretty err}
            PostCheckoutSessionsResponse200 session -> do
              pure $
                InitiatedCheckoutSession
                  { initiatedCheckoutSessionId = checkout'sessionId session,
                    initiatedCheckoutSessionCustomerId = customerId
                  }

mkPostCheckoutSessionsRequestBodyForUser :: InitiateStripeCheckoutSession -> Username -> Text -> Text -> PostCheckoutSessionsRequestBody
mkPostCheckoutSessionsRequestBodyForUser InitiateStripeCheckoutSession {..} username customerId planId =
  (mkPostCheckoutSessionsRequestBody initiateStripeCheckoutSessionSuccessUrl initiateStripeCheckoutSessionCanceledUrl)
    { postCheckoutSessionsRequestBodyCustomer = Just customerId,
      postCheckoutSessionsRequestBodyClientReferenceId = Just $ usernameText username,
      postCheckoutSessionsRequestBodyLineItems = Nothing,
      postCheckoutSessionsRequestBodyMode = Just PostCheckoutSessionsRequestBodyMode'EnumSubscription,
      postCheckoutSessionsRequestBodyMetadata = Just metadata,
      postCheckoutSessionsRequestBodySubscriptionData =
        Just $
          mkPostCheckoutSessionsRequestBodySubscriptionData'
            { postCheckoutSessionsRequestBodySubscriptionData'Metadata = Just metadata,
              postCheckoutSessionsRequestBodySubscriptionData'Items =
                Just
                  [ mkPostCheckoutSessionsRequestBodySubscriptionData'Items' planId
                  ]
            }
    }

getOrCreateCustomerId :: Stripe.Configuration -> User -> TicklerHandler Text
getOrCreateCustomerId config User {..} = do
  mStripeCustomer <- runDB $ selectFirst [StripeCustomerUser ==. userIdentifier] [Desc StripeCustomerId]
  case mStripeCustomer of
    Just (Entity _ sce) -> pure $ stripeCustomerCustomer sce
    Nothing -> do
      let postCustomersRequest = mkPostCustomersRequestBodyForUser userUsername
      resp <- liftIO $ runWithConfiguration config $ postCustomers $ Just postCustomersRequest
      case responseBody resp of
        PostCustomersResponseError err -> throwError err500 {errBody = LB.fromStrict $ TE.encodeUtf8 $ "Something went wrong while parsing stripe's response:\n" <> T.pack err}
        PostCustomersResponseDefault err -> throwError err500 {errBody = "Error while calling stripe:\n" <> JSON.encodePretty err}
        PostCustomersResponse200 Customer {..} -> do
          -- Keep track of it in our database for later
          _ <-
            runDB $
              upsertBy
                (UniqueStripeCustomer userIdentifier customerId)
                (StripeCustomer {stripeCustomerUser = userIdentifier, stripeCustomerCustomer = customerId})
                [StripeCustomerCustomer =. customerId]
          pure customerId

mkPostCustomersRequestBodyForUser :: Username -> PostCustomersRequestBody
mkPostCustomersRequestBodyForUser username =
  mkPostCustomersRequestBody
    { postCustomersRequestBodyDescription = Just $ usernameText username,
      postCustomersRequestBodyMetadata = Just $ PostCustomersRequestBodyMetadata'Object metadata
    }

metadata :: JSON.Object
metadata =
  KM.fromList
    [ ("product", "tickler"),
      ("tickler-server-version", toJSON $ showVersion version)
    ]
