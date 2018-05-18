{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Tickler.Web.Server.Foundation
    ( module Tickler.Web.Server.Foundation
    , module Tickler.Web.Server.Widget
    , module Tickler.Web.Server.Static
    , module Tickler.Web.Server.Constants
    ) where

import Import

import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T

import Control.Concurrent

import Control.Monad.Except
import Control.Monad.Trans.Maybe

import qualified Network.HTTP.Client as Http
import qualified Network.HTTP.Types as Http
import Web.Cookie

import Text.Hamlet
import Yesod hiding (Header)
import Yesod.Auth
import qualified Yesod.Auth.Message as Msg
import Yesod.EmbeddedStatic

import Servant.API
import Servant.Auth.Client (Token(..))
import Servant.Client

import Tickler.Client

import Tickler.Web.Server.Constants
import Tickler.Web.Server.Persistence
import Tickler.Web.Server.Static
import Tickler.Web.Server.Widget

type TicklerWidget = TicklerWidget' ()

type TicklerWidget' = WidgetFor App

type TicklerHandler = HandlerFor App

type TicklerAuthHandler a = AuthHandler App a

data App = App
    { appHttpManager :: Http.Manager
    , appStatic :: EmbeddedStatic
    , appAPIBaseUrl :: BaseUrl
    , appPersistLogins :: Bool
    , appLoginTokens :: MVar (HashMap Username Token)
    }

mkYesodData "App" $(parseRoutesFile "routes")

instance Yesod App where
    defaultLayout widget = do
        pc <- widgetToPageContent $(widgetFile "default-body")
        withUrlRenderer $(hamletFile "templates/default-page.hamlet")
    yesodMiddleware = defaultCsrfMiddleware . defaultYesodMiddleware
    authRoute _ = Just $ AuthR LoginR

instance PathPiece Username where
    fromPathPiece = parseUsername
    toPathPiece = usernameText

instance YesodAuth App where
    type AuthId App = Username
    loginDest _ = AddR
    logoutDest _ = HomeR
    authHttpManager = getsYesod appHttpManager
    authenticate creds =
        if credsPlugin creds == ticklerAuthPluginName
            then case parseUsername $ credsIdent creds of
                     Nothing -> pure $ UserError Msg.InvalidLogin
                     Just un -> pure $ Authenticated un
            else pure $
                 ServerError $
                 T.unwords ["Unknown authentication plugin:", credsPlugin creds]
    authPlugins _ = [ticklerAuthPlugin]
    maybeAuthId =
        runMaybeT $ do
            s <- MaybeT $ lookupSession credsKey
            MaybeT $ return $ fromPathPiece s

ticklerAuthPluginName :: Text
ticklerAuthPluginName = "tickler-auth-plugin"

ticklerAuthPlugin :: AuthPlugin App
ticklerAuthPlugin = AuthPlugin ticklerAuthPluginName dispatch loginWidget
  where
    dispatch :: Text -> [Text] -> TicklerAuthHandler TypedContent
    dispatch "POST" ["login"] = postLoginR >>= sendResponse
    dispatch "GET" ["register"] = getNewAccountR >>= sendResponse
    dispatch "POST" ["register"] = postNewAccountR >>= sendResponse
    dispatch _ _ = notFound
    loginWidget :: (Route Auth -> Route App) -> TicklerWidget
    loginWidget _ = do
        token <- genToken
        msgs <- getMessages
        $(widgetFile "auth/login")

data LoginData = LoginData
    { loginUserkey :: Text
    , loginPassword :: Text
    } deriving (Show)

loginFormPostTargetR :: AuthRoute
loginFormPostTargetR = PluginR ticklerAuthPluginName ["login"]

postLoginR :: TicklerAuthHandler TypedContent
postLoginR = do
    let loginInputForm =
            LoginData <$> ireq textField "userkey" <*>
            ireq passwordField "passphrase"
    result <- runInputPostResult loginInputForm
    muser <-
        case result of
            FormMissing -> invalidArgs ["Form is missing"]
            FormFailure _ -> return $ Left Msg.InvalidLogin
            FormSuccess (LoginData ukey pwd) ->
                case parseUsername ukey of
                    Nothing -> pure $ Left Msg.InvalidUsernamePass
                    Just un -> do
                        liftHandler $
                            login
                                LoginForm
                                { loginFormUsername = un
                                , loginFormPassword = pwd
                                }
                        pure $ Right un
    case muser of
        Left err -> loginErrorMessageI LoginR err
        Right un ->
            setCredsRedirect $ Creds ticklerAuthPluginName (usernameText un) []

registerR :: AuthRoute
registerR = PluginR ticklerAuthPluginName ["register"]

getNewAccountR :: TicklerAuthHandler Html
getNewAccountR = do
    token <- genToken
    msgs <- getMessages
    liftHandler $ defaultLayout $(widgetFile "auth/register")

data NewAccount = NewAccount
    { newAccountUsername :: Username
    , newAccountPassword1 :: Text
    , newAccountPassword2 :: Text
    } deriving (Show)

postNewAccountR :: TicklerAuthHandler TypedContent
postNewAccountR = do
    let newAccountInputForm =
            NewAccount <$>
            ireq
                (checkMMap
                     (\t ->
                          pure $
                          case parseUsernameWithError t of
                              Left err ->
                                  Left
                                      (T.pack $
                                       unwords
                                           [ "Invalid username:"
                                           , show t ++ ";"
                                           , err
                                           ])
                              Right un -> Right un)
                     usernameText
                     textField)
                "username" <*>
            ireq passwordField "passphrase" <*>
            ireq passwordField "passphrase-confirm"
    mr <- liftHandler getMessageRender
    result <- liftHandler $ runInputPostResult newAccountInputForm
    mdata <-
        case result of
            FormMissing -> invalidArgs ["Form is incomplete"]
            FormFailure msgs -> pure $ Left msgs
            FormSuccess d ->
                pure $
                if newAccountPassword1 d == newAccountPassword2 d
                    then Right
                             Registration
                             { registrationUsername = newAccountUsername d
                             , registrationPassword = newAccountPassword1 d
                             }
                    else Left [mr Msg.PassMismatch]
    case mdata of
        Left errs -> do
            setMessage $ toHtml $ T.concat errs
            liftHandler $ redirect $ AuthR registerR
        Right reg -> do
            errOrOk <- liftHandler $ runClient $ clientPostRegister reg
            case errOrOk of
                Left err -> do
                    case err of
                        FailureResponse resp ->
                            case Http.statusCode $ responseStatusCode resp of
                                409 ->
                                    setMessage
                                        "An account with this username already exists"
                                _ ->
                                    setMessage
                                        "Failed to register for unknown reasons."
                        _ ->
                            setMessage "Failed to register for unknown reasons."
                    liftHandler $ redirect $ AuthR registerR
                Right NoContent ->
                    liftHandler $ do
                        login
                            LoginForm
                            { loginFormUsername = registrationUsername reg
                            , loginFormPassword = registrationPassword reg
                            }
                        setCredsRedirect $
                            Creds
                                ticklerAuthPluginName
                                (usernameText $ registrationUsername reg)
                                []

instance RenderMessage App FormMessage where
    renderMessage _ _ = defaultFormMessage

instance PathPiece (UUID a) where
    fromPathPiece = parseUUID
    toPathPiece = uuidText

withNavBar :: WidgetFor App () -> HandlerFor App Html
withNavBar = withFormFailureNavBar []

withFormResultNavBar :: FormResult a -> WidgetFor App () -> HandlerFor App Html
withFormResultNavBar fr w =
    case fr of
        FormSuccess _ -> withNavBar w
        FormFailure ts -> withFormFailureNavBar ts w
        FormMissing -> withFormFailureNavBar ["Missing data"] w

withFormFailureNavBar :: [Text] -> Widget -> Handler Html
withFormFailureNavBar errs body = do
    mauth <- maybeAuthId
    msgs <- fmap (map ((,) "error" . toHtml) errs ++) getMessages
    defaultLayout $(widgetFile "with-nav-bar")

genToken :: MonadHandler m => m Html
genToken = do
    req <- getRequest
    let tokenKey = defaultCsrfParamName
    pure $
        case reqToken req of
            Nothing -> mempty
            Just n -> [shamlet|<input type=hidden name=#{tokenKey} value=#{n}>|]

runClient :: ClientM a -> Handler (Either ServantError a)
runClient func = do
    man <- getsYesod appHttpManager
    burl <- getsYesod appAPIBaseUrl
    let cenv = ClientEnv man burl Nothing
    liftIO $ runClientM func cenv

runClientOrErr :: ClientM a -> Handler a
runClientOrErr func = do
    errOrRes <- runClient func
    case errOrRes of
        Left err ->
            handleStandardServantErrs err $ \resp ->
                error $ show resp -- TODO deal with error
        Right r -> pure r

handleStandardServantErrs ::
       ServantError -> (Response -> Handler a) -> Handler a
handleStandardServantErrs err func =
    case err of
        FailureResponse resp -> func resp
        ConnectionError e -> redirect $ ErrorAPIDownR $ T.pack $ show e
        e -> error $ unwords ["Error while calling API:", show e]

login :: LoginForm -> Handler ()
login form = do
    errOrRes <- runClient $ clientPostLogin form
    case errOrRes of
        Left err ->
            handleStandardServantErrs err $ \resp ->
                if responseStatusCode resp == Http.unauthorized401
                    then do
                        addMessage "error" "Unable to login"
                        redirect $ AuthR LoginR
                    else error $ show resp
        Right (Headers NoContent (HCons _ (HCons sessionHeader HNil))) ->
            case sessionHeader of
                Header session ->
                    recordLoginToken (loginFormUsername form) session
                _ -> undefined -- TODO deal with this error

withLogin :: (Token -> Handler Html) -> Handler Html
withLogin func = do
    un <- requireAuthId
    mLoginToken <- lookupToginToken un
    case mLoginToken of
        Nothing -> redirect $ AuthR LoginR
        Just token -> func token

lookupToginToken :: Username -> Handler (Maybe Token)
lookupToginToken un = do
    whenPersistLogins loadLogins
    tokenMapVar <- getsYesod appLoginTokens
    tokenMap <- liftIO $ readMVar tokenMapVar
    pure $ HM.lookup un tokenMap

recordLoginToken :: Username -> SetCookie -> Handler ()
recordLoginToken un session = do
    let token = Token $ setCookieValue session
    tokenMapVar <- getsYesod appLoginTokens
    liftIO $ modifyMVar_ tokenMapVar $ pure . HM.insert un token
    whenPersistLogins storeLogins

whenPersistLogins :: Handler () -> Handler ()
whenPersistLogins f = do
    b <- getsYesod appPersistLogins
    when b f

loadLogins :: Handler ()
loadLogins = do
    tokenMapVar <- getsYesod appLoginTokens
    liftIO $ modifyMVar_ tokenMapVar $ \m -> fromMaybe m <$> readLogins

storeLogins :: Handler ()
storeLogins = do
    tokenMapVar <- getsYesod appLoginTokens
    liftIO $ do
        m <- readMVar tokenMapVar
        writeLogins m
