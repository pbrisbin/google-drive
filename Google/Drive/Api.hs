-- |
--
-- Low-level interface to the Google Drive API
--
module Network.Google.Drive.Api
    ( Api
    , ApiError(..)
    , runApi
    , Path
    , Params
    , URL

    -- * Actions
    , getApi
    , postApi

    -- * Logging and errors
    , logApi
    , throwApiError

    -- * Utilities
    , authenticatedDownload
    , authorize
    , addHeader
    , setBody
    , setMethod
    , decodeBody
    , tryHttp

    -- * Re-exports
    , liftIO
    ) where

import Control.Monad.Error
import Control.Monad.Reader
import Data.Aeson (FromJSON(..), ToJSON(..), eitherDecode, encode)
import Data.ByteString (ByteString)
import Data.Conduit
import Data.Conduit.Binary (sinkFile)
import Data.Conduit.Progress
import Data.Monoid ((<>))
import Network.HTTP.Conduit
    ( HttpException(..)
    , Request(..)
    , RequestBody(..)
    , http
    , httpLbs
    , parseUrl
    , responseBody
    , setQueryString
    , withManager
    )
import Network.HTTP.Types (Header, Method, hAuthorization, hContentType)
import System.Directory (createDirectoryIfMissing)
import System.FilePath (takeDirectory)

import qualified Control.Exception as E
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C8
import qualified Data.ByteString.Lazy as BL

data ApiError
    = HttpError HttpException
    | InvalidJSON String BL.ByteString
    | GenericError String

instance Error ApiError where
    strMsg = GenericError

instance Show ApiError where
    show (HttpError ex) = "HTTP Exception: " <> show ex
    show (InvalidJSON msg _) = "Error parsing JSON: " <> msg
    show (GenericError str) = "Error: " <> str

type Api a = ReaderT String (ErrorT ApiError IO) a

runApi :: String -> Api a -> IO (Either ApiError a)
runApi token f = runErrorT $ runReaderT f token

-- | Print a string to stdout (temporary)
logApi :: String -> Api ()
logApi = liftIO . putStrLn

-- | Abort with the given error message
throwApiError :: String -> Api ()
throwApiError = throwError . GenericError

type URL = String
type Path = String
type Params = [(ByteString, Maybe ByteString)]

getApi :: FromJSON a => Path -> Params -> Api a
getApi path query = do
    request <- fmap (setQueryString query) $ authorize =<< apiRequest path

    decodeBody . responseBody =<< tryHttp (withManager $ httpLbs request)

postApi :: (ToJSON a, FromJSON b) => Path -> a -> Api b
postApi path body = do
    let content = (hContentType, "application/json")
        modify = addHeader content . setBody (encode body) . setMethod "POST"

    request <- fmap modify $ authorize =<< apiRequest path

    decodeBody . responseBody =<< tryHttp (withManager $ httpLbs request)

authenticatedDownload :: URL -> (Maybe Int) -> FilePath -> Api ()
authenticatedDownload url msize path = do
    request <- authorize =<< (liftIO $ parseUrl url)

    liftIO $ createDirectoryIfMissing True $ takeDirectory path

    let sink = case msize of
            Nothing -> sinkFile path
            Just size -> reportProgress B.length size 100 =$ sinkFile path

    tryHttp $ withManager $ \manager -> do
        response <- http request manager
        responseBody response $$+- sink

-- | Authorize a request with our OAuthToken
authorize :: Request -> Api Request
authorize request = do
    token <- ask

    let authorization = C8.pack $ "Bearer " <> token

    return $ addHeader (hAuthorization, authorization) request

addHeader :: Header -> Request -> Request
addHeader header request =
    request { requestHeaders = header:requestHeaders request }

setBody :: BL.ByteString -> Request -> Request
setBody bs request = request { requestBody = RequestBodyLBS bs }

setMethod :: Method -> Request -> Request
setMethod method request = request { method = method }

-- | Decode to JSON, re-throwing any parsing errors as an @'ApiError'@
decodeBody :: FromJSON a => BL.ByteString -> Api a
decodeBody body = case eitherDecode body of
    Left ex -> throwError $ InvalidJSON ex body
    Right x -> return x

-- | Attempt the (presumably HTTP-related) action, re-throwing any raised
--   @'HttpException'@s exceptions as an @'ApiError'@.
tryHttp :: IO a -> Api a
tryHttp f = do
    result <- liftIO $ E.try f

    case result of
        Left ex -> throwError $ HttpError ex
        Right x -> return x

baseUrl :: URL
baseUrl = "https://www.googleapis.com/drive/v2"

apiRequest :: Path -> Api Request
apiRequest path = liftIO $ parseUrl $ baseUrl <> path
