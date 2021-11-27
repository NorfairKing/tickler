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
  ( module Tickler.Web.Server.Foundation,
    module Tickler.Web.Server.Widget,
    module Tickler.Web.Server.Static,
    module Tickler.Web.Server.Constants,
  )
where

import Control.Concurrent
import Control.Monad.Trans.Maybe
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)
import Import
import qualified Network.HTTP.Client as Http
import qualified Network.HTTP.Types as Http
import Servant.API
import Servant.Auth.Client (Token (..))
import Servant.Client
import Text.Hamlet
import Tickler.Client
import Tickler.Web.Server.Constants
import Tickler.Web.Server.Persistence
import Tickler.Web.Server.Static
import Tickler.Web.Server.Widget
import Web.Cookie
import Yesod hiding (Header)
import Yesod.Auth
import qualified Yesod.Auth.Message as Msg
import Yesod.EmbeddedStatic

type TicklerWidget = TicklerWidget' ()

type TicklerWidget' = WidgetFor App

type TicklerHandler = HandlerFor App

type TicklerAuthHandler a = AuthHandler App a

data App = App
  { appHttpManager :: Http.Manager,
    appStatic :: EmbeddedStatic,
    appAPIBaseUrl :: BaseUrl,
    appPersistLogins :: Bool,
    appTracking :: Maybe Text,
    appVerification :: Maybe Text,
    appLoginTokens :: MVar (HashMap Username Token),
    appDefaultIntrayUrl :: Maybe BaseUrl
  }

mkYesodData "App" $(parseRoutesFile "routes")

instance Yesod App where
  defaultLayout widget = do
    app_ <- getYesod
    pc <- widgetToPageContent $ do
      toWidgetHead [hamlet|<link rel="icon" href=@{StaticR static_favicon_ico} sizes="16x16 24x24 32x32 48x48 64x64" type="image/x-icon">|]
      $(widgetFile "default-body")
    withUrlRenderer $(hamletFile "templates/default-page.hamlet")
  yesodMiddleware = defaultCsrfMiddleware . defaultYesodMiddleware
  authRoute _ = Just $ AuthR LoginR
  makeSessionBackend _ =
    Just <$> defaultClientSessionBackend (60 * 24 * 365 * 10) "client_session_key.aes"
  errorHandler NotFound =
    fmap toTypedContent $
      withNavBar $
        do
          setTitle "Page not found"
          [whamlet|
      <h1>
        Page not found
      |]
  errorHandler other = defaultErrorHandler other

instance PathPiece Username where
  fromPathPiece = parseUsername
  toPathPiece = usernameText

instance PathPiece EmailVerificationKey where
  fromPathPiece = parseEmailVerificationKeyText
  toPathPiece = emailVerificationKeyText

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
      else pure $ ServerError $ T.unwords ["Unknown authentication plugin:", credsPlugin creds]
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
    dispatch "GET" ["change-password"] = getChangePasswordR >>= sendResponse
    dispatch "POST" ["change-password"] = postChangePasswordR >>= sendResponse
    dispatch _ _ = notFound
    loginWidget :: (Route Auth -> Route App) -> TicklerWidget
    loginWidget _ = do
      token <- genToken
      msgs <- getMessages
      setTitle "Tickler Login"
      setDescription "This is where you sign into your tickler account."
      $(widgetFile "auth/login")

data LoginData = LoginData
  { loginUserkey :: Text,
    loginPassword :: Text
  }
  deriving (Show)

loginFormPostTargetR :: AuthRoute
loginFormPostTargetR = PluginR ticklerAuthPluginName ["login"]

postLoginR :: TicklerAuthHandler TypedContent
postLoginR = do
  let loginInputForm = LoginData <$> ireq textField "userkey" <*> ireq passwordField "passphrase"
  result <- runInputPostResult loginInputForm
  muser <-
    case result of
      FormMissing -> invalidArgs ["Form is missing"]
      FormFailure _ -> return $ Left Msg.InvalidLogin
      FormSuccess (LoginData ukey pwd) ->
        case parseUsername ukey of
          Nothing -> pure $ Left Msg.InvalidUsernamePass
          Just un -> do
            liftHandler $ login LoginForm {loginFormUsername = un, loginFormPassword = pwd}
            pure $ Right un
  case muser of
    Left err -> loginErrorMessageI LoginR err
    Right un -> setCredsRedirect $ Creds ticklerAuthPluginName (usernameText un) []

registerR :: AuthRoute
registerR = PluginR ticklerAuthPluginName ["register"]

getNewAccountR :: TicklerAuthHandler Html
getNewAccountR = do
  token <- genToken
  msgs <- getMessages
  liftHandler $
    defaultLayout $
      do
        setTitle "Tickler Login"
        setDescription "This is where you sign into your tickler account."
        $(widgetFile "auth/register")

data NewAccount = NewAccount
  { newAccountUsername :: Username,
    newAccountPassword1 :: Text,
    newAccountPassword2 :: Text
  }
  deriving (Show)

postNewAccountR :: TicklerAuthHandler TypedContent
postNewAccountR = do
  let newAccountInputForm =
        NewAccount
          <$> ireq
            ( checkMMap
                ( \t ->
                    pure $
                      case parseUsernameWithError t of
                        Left err -> Left (T.pack $ unwords ["Invalid username:", show t ++ ";", err])
                        Right un -> Right un
                )
                usernameText
                textField
            )
            "username"
          <*> ireq passwordField "passphrase"
          <*> ireq passwordField "passphrase-confirm"
  mr <- liftHandler getMessageRender
  result <- liftHandler $ runInputPostResult newAccountInputForm
  mdata <-
    case result of
      FormMissing -> invalidArgs ["Form is incomplete"]
      FormFailure msgs -> pure $ Left msgs
      FormSuccess d ->
        pure $
          if newAccountPassword1 d == newAccountPassword2 d
            then
              Right
                Registration
                  { registrationUsername = newAccountUsername d,
                    registrationPassword = newAccountPassword1 d
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
            FailureResponse _ resp ->
              case Http.statusCode $ responseStatusCode resp of
                409 -> addMessage "error" "An account with this username already exists"
                c ->
                  addMessage "error" $
                    "Failed to register for unknown reasons, got status code: " <> toHtml c
            ConnectionError t ->
              addMessage "error" $
                "Failed to register for unknown reasons. with connection error:" <> toHtml (show t)
            e ->
              addMessage "error" $
                "Failed to register for unknown reasons. with error:" <> toHtml (show e)
          liftHandler $ redirect $ AuthR registerR
        Right NoContent ->
          liftHandler $ do
            login
              LoginForm
                { loginFormUsername = registrationUsername reg,
                  loginFormPassword = registrationPassword reg
                }
            setCredsRedirect $
              Creds ticklerAuthPluginName (usernameText $ registrationUsername reg) []

changePasswordTargetR :: AuthRoute
changePasswordTargetR = PluginR ticklerAuthPluginName ["change-password"]

data ChangePassword = ChangePassword
  { changePasswordOldPassword :: Text,
    changePasswordNewPassword1 :: Text,
    changePasswordNewPassword2 :: Text
  }
  deriving (Show)

getChangePasswordR :: TicklerAuthHandler Html
getChangePasswordR = do
  token <- genToken
  msgs <- getMessages
  liftHandler $ defaultLayout $(widgetFile "auth/change-password")

postChangePasswordR :: TicklerAuthHandler Html
postChangePasswordR = do
  ChangePassword {..} <-
    liftHandler $
      runInputPost $
        ChangePassword <$> ireq passwordField "old" <*> ireq passwordField "new1"
          <*> ireq passwordField "new2"
  unless (changePasswordNewPassword1 == changePasswordNewPassword2) $
    invalidArgs ["Passwords do not match."]
  liftHandler $
    withLogin $
      \t -> do
        let cpp =
              ChangePassphrase
                { changePassphraseOld = changePasswordOldPassword,
                  changePassphraseNew = changePasswordNewPassword1
                }
        NoContent <- runClientOrErr $ clientPostChangePassphrase t cpp
        redirect AccountR

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
  alreadyExpired
  req <- getRequest
  let tokenKey = defaultCsrfParamName
  pure $
    case reqToken req of
      Nothing -> mempty
      Just n -> [shamlet|<input type=hidden name=#{tokenKey} value=#{n}>|]

runClient :: ClientM a -> Handler (Either ClientError a)
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
      handleStandardServantErrs err $ \resp -> sendResponseStatus Http.status500 $ show resp
    Right r -> pure r

runClientOrDisallow :: ClientM a -> Handler (Maybe a)
runClientOrDisallow func = do
  errOrRes <- runClient func
  case errOrRes of
    Left err ->
      handleStandardServantErrs err $ \resp ->
        if responseStatusCode resp == Http.unauthorized401
          then pure Nothing
          else sendResponseStatus Http.status500 $ show resp
    Right r -> pure $ Just r

handleStandardServantErrs :: ClientError -> (Response -> Handler a) -> Handler a
handleStandardServantErrs err func =
  case err of
    FailureResponse _ resp -> func resp
    ConnectionError e -> redirect $ ErrorAPIDownR $ T.pack $ show e
    e -> sendResponseStatus Http.status500 $ unwords ["Error while calling API:", show e]

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
          else sendResponseStatus Http.status500 $ show resp
    Right (Headers NoContent (HCons sessionHeader HNil)) ->
      case sessionHeader of
        Header session -> recordLoginToken (loginFormUsername form) session
        _ ->
          sendResponseStatus Http.status500 $
            unwords ["The server responded but with an invalid header for login", show sessionHeader]

withLogin :: (Token -> Handler a) -> Handler a
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

recordLoginToken :: Username -> Text -> Handler ()
recordLoginToken un session = do
  let token = Token $ setCookieValue $ parseSetCookie $ encodeUtf8 session
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

addInfoMessage :: Html -> Handler ()
addInfoMessage = addMessage ""

addNegativeMessage :: Html -> Handler ()
addNegativeMessage = addMessage "negative"

addPositiveMessage :: Html -> Handler ()
addPositiveMessage = addMessage "positive"
