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

import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Import
import qualified Network.HTTP.Client as HTTP
import qualified Network.HTTP.Types as HTTP
import Servant.API
import Servant.Auth.Client (Token (..))
import Servant.Client
import Text.Hamlet
import Tickler.Client
import Tickler.Web.Server.Constants
import Tickler.Web.Server.Static
import Tickler.Web.Server.Widget
import Web.Cookie
import Yesod hiding (Header)
import Yesod.Auth
import qualified Yesod.Auth.Message as Msg
import Yesod.AutoReload
import Yesod.EmbeddedStatic

type TicklerWidget = TicklerWidget' ()

type TicklerWidget' = WidgetFor App

type TicklerHandler = HandlerFor App

type TicklerAuthHandler a = AuthHandler App a

data App = App
  { appHTTPManager :: !HTTP.Manager,
    appLogLevel :: !LogLevel,
    appStatic :: !EmbeddedStatic,
    appAPIBaseUrl :: !BaseUrl,
    appTracking :: !(Maybe Text),
    appVerification :: !(Maybe Text),
    appSessionKeyFile :: !(Path Abs File),
    appDefaultIntrayUrl :: !(Maybe BaseUrl)
  }

mkYesodData "App" $(parseRoutesFile "routes")

instance Yesod App where
  defaultLayout widget = do
    app_ <- getYesod
    pc <- widgetToPageContent $ do
      toWidgetHead [hamlet|<link rel="icon" href=@{StaticR static_favicon_ico} sizes="16x16 24x24 32x32 48x48 64x64" type="image/x-icon">|]
      let withAutoreload = if development then (<> autoReloadWidgetFor ReloadR) else id
      withAutoreload $(widgetFile "default-body")
    withUrlRenderer $(hamletFile "templates/default-page.hamlet")
  yesodMiddleware = defaultCsrfMiddleware . defaultYesodMiddleware
  authRoute _ = Just $ AuthR LoginR
  makeSessionBackend a = Just <$> defaultClientSessionBackend (60 * 24 * 365 * 10) (fromAbsFile (appSessionKeyFile a))
  shouldLogIO a _ ll = pure $ ll >= appLogLevel a
  errorHandler NotFound =
    fmap toTypedContent
      $ withNavBar
      $ do
        setTitle "Page not found"
        [whamlet|
      <h1>
        Page not found
      |]
  errorHandler other = defaultErrorHandler other

instance PathPiece EmailVerificationKey where
  fromPathPiece = parseEmailVerificationKeyText
  toPathPiece = emailVerificationKeyText

instance YesodAuth App where
  type AuthId App = Text -- Session token, but Text instead of Token because we need a 'PathPiece' instance.
  loginDest _ = AddR
  logoutDest _ = HomeR
  authHttpManager = getsYesod appHTTPManager
  authenticate creds =
    pure
      $ if credsPlugin creds == ticklerAuthPluginName
        then Authenticated $ credsIdent creds
        else ServerError $ T.unwords ["Unknown authentication plugin:", credsPlugin creds]
  authPlugins _ = [ticklerAuthPlugin]
  maybeAuthId = lookupSession credsKey

ticklerAuthPluginName :: Text
ticklerAuthPluginName = "tickler-auth-plugin"

{-# ANN ticklerAuthPlugin ("NOCOVER" :: String) #-}
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
      setDescriptionIdemp "This is where you sign into your tickler account."
      $(widgetFile "auth/login")

data LoginData = LoginData
  { loginUsername :: Text,
    loginPassword :: Text
  }
  deriving (Show)

loginFormPostTargetR :: AuthRoute
loginFormPostTargetR = PluginR ticklerAuthPluginName ["login"]

{-# ANN postLoginR ("NOCOVER" :: String) #-}
postLoginR :: TicklerAuthHandler TypedContent
postLoginR = do
  let loginInputForm = LoginData <$> ireq textField "username" <*> ireq passwordField "passphrase"
  result <- runInputPostResult loginInputForm
  muser <-
    case result of
      FormMissing -> invalidArgs ["Form is missing"]
      FormFailure _ -> return $ Left Msg.InvalidLogin
      FormSuccess (LoginData name pwd) ->
        case parseUsername name of
          Nothing -> pure $ Left Msg.InvalidUsernamePass
          Just un -> do
            session <- liftHandler $ login LoginForm {loginFormUsername = un, loginFormPassword = pwd}
            pure $ Right session
  case muser of
    Left err -> loginErrorMessageI LoginR err
    Right session -> setCredsRedirect $ Creds ticklerAuthPluginName session []

registerR :: AuthRoute
registerR = PluginR ticklerAuthPluginName ["register"]

getNewAccountR :: TicklerAuthHandler Html
getNewAccountR = do
  token <- genToken
  msgs <- getMessages
  liftHandler
    $ defaultLayout
    $ do
      setTitle "Tickler Login"
      setDescriptionIdemp "This is where you sign into your tickler account."
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
                    pure
                      $ case parseUsernameWithError t of
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
        pure
          $ if newAccountPassword1 d == newAccountPassword2 d
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
              case HTTP.statusCode $ responseStatusCode resp of
                409 -> addMessage "error" "An account with this username already exists"
                c ->
                  addMessage "error"
                    $ "Failed to register for unknown reasons, got status code: "
                    <> toHtml c
            ConnectionError t ->
              addMessage "error"
                $ "Failed to register for unknown reasons. with connection error:"
                <> toHtml (show t)
            e ->
              addMessage "error"
                $ "Failed to register for unknown reasons. with error:"
                <> toHtml (show e)
          liftHandler $ redirect $ AuthR registerR
        Right NoContent ->
          liftHandler $ do
            session <-
              login
                LoginForm
                  { loginFormUsername = registrationUsername reg,
                    loginFormPassword = registrationPassword reg
                  }
            setCredsRedirect
              $ Creds ticklerAuthPluginName session []

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
    liftHandler
      $ runInputPost
      $ ChangePassword
      <$> ireq passwordField "old"
      <*> ireq passwordField "new1"
      <*> ireq passwordField "new2"
  unless (changePasswordNewPassword1 == changePasswordNewPassword2)
    $ invalidArgs ["Passwords do not match."]
  liftHandler
    $ withLogin
    $ \t -> do
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
  fromPathPiece = parseUUIDText
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
  currentRoute <- getCurrentRoute
  defaultLayout $(widgetFile "with-nav-bar")

genToken :: (MonadHandler m) => m Html
genToken = genToken_ defaultCsrfParamName

genToken_ :: (MonadHandler m) => Text -> m Html
genToken_ tokenKey = do
  alreadyExpired
  req <- getRequest
  pure
    $ case reqToken req of
      Nothing -> mempty
      Just n -> [shamlet|<input type=hidden name=#{tokenKey} value=#{n}>|]

runClient :: ClientM a -> Handler (Either ClientError a)
runClient func = do
  man <- getsYesod appHTTPManager
  burl <- getsYesod appAPIBaseUrl
  let cenv = mkClientEnv man burl
  liftIO $ runClientM func cenv

runClientOrErr :: ClientM a -> Handler a
runClientOrErr func = do
  errOrRes <- runClient func
  case errOrRes of
    Left err ->
      handleStandardServantErrs err $ \resp -> sendResponseStatus HTTP.status500 $ show resp
    Right r -> pure r

runClientOrDisallow :: ClientM a -> Handler (Maybe a)
runClientOrDisallow func = do
  errOrRes <- runClient func
  case errOrRes of
    Left err ->
      handleStandardServantErrs err $ \resp ->
        if responseStatusCode resp == HTTP.unauthorized401
          then pure Nothing
          else sendResponseStatus HTTP.status500 $ show resp
    Right r -> pure $ Just r

handleStandardServantErrs :: ClientError -> (Response -> Handler a) -> Handler a
handleStandardServantErrs err func =
  case err of
    FailureResponse _ resp -> func resp
    ConnectionError e ->
      sendResponseStatus HTTP.status500
        $ unwords
          [ "Error while connecting to API:",
            show e
          ]
    e ->
      sendResponseStatus HTTP.status500
        $ unwords
          [ "Error while calling API:",
            show e
          ]

login :: LoginForm -> Handler Text
login form = do
  errOrRes <- runClient $ clientPostLogin form
  case errOrRes of
    Left err ->
      handleStandardServantErrs err $ \resp ->
        if responseStatusCode resp == HTTP.unauthorized401
          then do
            addMessage "error" "Unable to login"
            redirect $ AuthR LoginR
          else sendResponseStatus HTTP.status500 $ show resp
    Right (Headers NoContent (HCons sessionHeader HNil)) ->
      case sessionHeader of
        Header session -> do
          setCreds False $ Creds ticklerAuthPluginName session []
          pure session
        _ ->
          sendResponseStatus HTTP.status500
            $ unwords
              [ "The server responded but with an invalid header for login",
                show sessionHeader
              ]

withLogin :: (Token -> Handler a) -> Handler a
withLogin func = requireAuthId >>= (func . sessionToToken)

sessionToToken :: Text -> Token
sessionToToken = Token . setCookieValue . parseSetCookie . TE.encodeUtf8

addInfoMessage :: Html -> Handler ()
addInfoMessage = addMessage ""

addNegativeMessage :: Html -> Handler ()
addNegativeMessage = addMessage "danger"

addPositiveMessage :: Html -> Handler ()
addPositiveMessage = addMessage "success"

minuteOfDayField ::
  forall m.
  ( Monad m,
    RenderMessage (HandlerSite m) FormMessage
  ) =>
  Field m MinuteOfDay
minuteOfDayField =
  checkMMap
    (pure . (Right :: MinuteOfDay -> Either FormMessage MinuteOfDay) . timeOfDayToMinuteOfDay)
    minuteOfDayToTimeOfDay
    timeField
