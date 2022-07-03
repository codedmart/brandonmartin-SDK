module Web.TheOne where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Bifunctor (first)
import Data.Proxy
import Data.Text (Text, unpack)
import Network.HTTP.Client (newManager)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Servant.API hiding (addHeader)
import Servant.Client
import Servant.Client.Core

import Web.TheOne.Types

{--
  TODO:
    Handle filtering
    Documentation
    Setup tests
--}

type instance AuthClientData (AuthProtect "auth") = String

authenticateReq :: String -> Request -> Request
authenticateReq s r =
  addHeader "Authorization" ("Bearer " <> s) r

type TheOneAuthAPI
  = AuthProtect "auth" :> TheOneAPI

type TheOneListRoute a
  = QueryParam "limit" Int
  :> QueryParam "page" Int
  :> QueryParam "offset" Int
  :> QueryParam "sort" Text
  :> Get '[JSON] (Entities a)

type TheOneAPI
  = "book"
    :> TheOneListRoute Book
  :<|> "book"
    :> Capture "book_id" Text
    :> Get '[JSON] (Entity Book)
  :<|> "book"
    :> Capture "book_id" Text
    :> "chapter"
    :> Get '[JSON] (Entities Chapter)
  :<|> "movie"
    :> TheOneListRoute Movie
  :<|> "movie"
    :> Capture "movie_id" Text
    :> Get '[JSON] (Entity Movie)
  :<|> "movie"
    :> Capture "movie_id" Text
    :> "quote"
    :> Get '[JSON] (Entities Quote)
  :<|> "character"
    :> TheOneListRoute Character
  :<|> "character"
    :> Capture "character_id" Text
    :> Get '[JSON] (Entity Character)
  :<|> "character"
    :> Capture "character_id" Text
    :> Get '[JSON] (Entities Quote)
  :<|> "quote"
    :> TheOneListRoute Quote
  :<|> "quote"
    :> Capture "quote_id" Text
    :> Get '[JSON] (Entity Quote)
  :<|> "chapter"
    :> TheOneListRoute Chapter
  :<|> "chapter"
    :> Capture "chapter_id" Text
    :> Get '[JSON] (Entity Chapter)

type TheOneListFunc a
  = Maybe Int
  -> Maybe Int
  -> Maybe Int
  -> Maybe Text
  -> ClientM (Entities a)

data TheOneClient = TheOneClient
  { books
      :: TheOneListFunc Book
  , book
      :: Text
      -> ClientM (Entity Book)
  , bookChapters
      :: Text
      -> ClientM (Entities Chapter)
  , movies
      :: TheOneListFunc Movie
  , movie
      :: Text
      -> ClientM (Entity Movie)
  , movieQuotes
      :: Text
      -> ClientM (Entities Quote)
  , characters
      :: TheOneListFunc Character
  , character
      :: Text
      -> ClientM (Entity Character)
  , characterQuotes
      :: Text
      -> ClientM (Entities Quote)
  , quotes
      :: TheOneListFunc Quote
  , quote
      :: Text
      -> ClientM (Entity Quote)
  , chapters
      :: TheOneListFunc Chapter
  , chapter
      :: Text
      -> ClientM (Entity Chapter)
  }

mkTheOneClient :: TheOneEnv -> TheOneClient
mkTheOneClient i = TheOneClient {..}
  where
    books
      :<|> book
      :<|> bookChapters
      :<|> movies
      :<|> movie
      :<|> movieQuotes
      :<|> characters
      :<|> character
      :<|> characterQuotes
      :<|> quotes
      :<|> quote
      :<|> chapters
      :<|> chapter
      = client (Proxy @TheOneAuthAPI) auth
    accessToken = unAccessToken $ theOneAccessToken i
    auth :: AuthenticatedRequest (AuthProtect "auth")
    auth = mkAuthenticatedRequest accessToken authenticateReq

theOneBaseUrl :: BaseUrl
theOneBaseUrl = BaseUrl Https "the-one-api.dev" 443 "/v2"

theOneAPI :: Proxy TheOneAPI
theOneAPI = Proxy

mkTheOneEnv :: Text -> IO TheOneEnv
mkTheOneEnv at = do
  mgr <- newManager tlsManagerSettings
  return $ TheOneEnv mgr (AccessToken $ unpack at)

runTheOne
  :: MonadIO m
  => TheOneEnv
  -> ClientM a
  -> m (Either TheOneError a)
runTheOne m = fmap (first toTheOneError)
  . liftIO
  . flip runClientM (mkClientEnv (theOneManager m) theOneBaseUrl)