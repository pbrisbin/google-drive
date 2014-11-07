-- |
--
-- Actions for working with any of Google's APIs
--
module Network.Google.Api
    ( Api
    , ApiError(..)
    , URL
    , Path
    , Params
    , runApi
    , throwApiError

    -- * High-level requests
    , getJSON
    , getSource
    , postJSON

    -- * Lower-level request
    , requestJSON
    , requestLbs

    -- * Api helpers
    , decodeBody

    -- * Request helpers
    , addHeader
    , setMethod
    , setBody
    , setBodySource

    -- * Re-exports
    , liftIO
    , catchError
    ) where

import Control.Monad.Error
import Control.Monad.Reader
import Control.Monad.Trans.Resource
import Data.Aeson (FromJSON(..), ToJSON(..), eitherDecode, encode)
import Data.ByteString (ByteString)
import Data.Conduit
import Data.Monoid ((<>))
import GHC.Int (Int64)
import Network.HTTP.Conduit
    ( HttpException(..)
    , Request(..)
    , RequestBody(..)
    , Response(..)
    , http
    , httpLbs
    , parseUrl
    , requestBodySource
    , responseBody
    , setQueryString
    , withManager
    )
import Network.HTTP.Types (Header, Method, hAuthorization, hContentType)

import qualified Control.Exception as E
import qualified Data.ByteString.Char8 as C8
import qualified Data.ByteString.Lazy as BL

type SourceHandler a =
    ResumableSource (ResourceT IO) ByteString -> ResourceT IO a

data ApiError
    = HttpError HttpException
    | InvalidJSON String
    | GenericError String

instance Error ApiError where
    strMsg = GenericError

instance Show ApiError where
    show (HttpError ex) = "HTTP Exception: " <> show ex
    show (InvalidJSON msg) = "failure parsing JSON: " <> msg
    show (GenericError msg) = msg

type Api a = ReaderT String (ErrorT ApiError IO) a

runApi :: String -> Api a -> IO (Either ApiError a)
runApi token f = runErrorT $ runReaderT f token

-- | Abort with the given error message
throwApiError :: String -> Api a
throwApiError = throwError . GenericError

type URL = String
type Path = String
type Params = [(ByteString, Maybe ByteString)]

getJSON :: FromJSON a => URL -> Params -> Api a
getJSON url params = requestJSON url $ setQueryString params

getSource :: URL -> Params -> SourceHandler a -> Api a
getSource url params withSource = do
    request <- fmap (setQueryString params) $ authorize url

    tryHttp $ withManager $ \manager -> do
        response <- http request manager
        withSource $ responseBody response

postJSON :: (ToJSON a, FromJSON b) => URL -> Params -> a -> Api b
postJSON url params body =
    requestJSON url $
        addHeader (hContentType, "application/json") .
        setMethod "POST" .
        setQueryString params .
        setBody (encode body)

requestJSON :: FromJSON a => URL -> (Request -> Request) -> Api a
requestJSON url modify = decodeBody =<< requestLbs url modify

requestLbs :: URL -> (Request -> Request) -> Api (Response BL.ByteString)
requestLbs url modify = do
    request <- authorize url

    tryHttp $ withManager $ httpLbs $ modify request

authorize :: URL -> Api Request
authorize url = do
    token <- ask
    request <- liftIO $ parseUrl url -- TODO: handle with Either

    let authorization = C8.pack $ "Bearer " <> token

    return $ addHeader (hAuthorization, authorization) request

addHeader :: Header -> Request -> Request
addHeader header request =
    request { requestHeaders = header:requestHeaders request }

setMethod :: Method -> Request -> Request
setMethod method request = request { method = method }

setBody :: BL.ByteString -> Request -> Request
setBody bs request = request { requestBody = RequestBodyLBS bs }

setBodySource :: Int64 -> Source (ResourceT IO) ByteString -> Request -> Request
setBodySource len source request =
    request { requestBody = requestBodySource len source }

decodeBody :: FromJSON a => Response BL.ByteString -> Api a
decodeBody response =
    case eitherDecode $ responseBody response of
        Left ex -> throwError $ InvalidJSON ex
        Right x -> return x

tryHttp :: IO a -> Api a
tryHttp f = do
    result <- liftIO $ E.try f

    case result of
        Left ex -> throwError $ HttpError ex
        Right x -> return x
