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
    , UploadSource
    , DownloadSink
    , getJSON
    , getSource
    , postJSON

    -- * Lower-level request
    , requestJSON
    , requestLbs

    -- * Api helpers
    , decodeBody
    , debugApi

    -- * Request helpers
    , addHeader
    , setMethod
    , setBody
    , setBodySource
    , allowStatus

    -- * Re-exports
    , def
    , liftIO
    , throwError
    , catchError
    ) where

import Control.Applicative ((<$>))
import Control.Monad.Error
import Control.Monad.Reader
import Control.Monad.Trans.Resource
import Data.Aeson (FromJSON(..), ToJSON(..), eitherDecode, encode)
import Data.ByteString (ByteString)
import Data.Conduit
import Data.Default (Default(..))
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
import Network.HTTP.Types (Header, Method, Status, hAuthorization, hContentType)
import System.IO (hPutStrLn, stderr)

import qualified Control.Exception as E
import qualified Data.ByteString.Char8 as C8
import qualified Data.ByteString.Lazy as BL

type UploadSource = Int -> Source (ResourceT IO) ByteString

type DownloadSink a =
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

data ApiConfig = ApiConfig
    { apiToken :: String
    , apiDebug :: Bool
    }

type Api a = ReaderT ApiConfig (ErrorT ApiError IO) a

runApi :: String -> Bool -> Api a -> IO (Either ApiError a)
runApi token debug f = runErrorT $ runReaderT action $ ApiConfig token debug
  where
    action = debugApi ("Using token: " <> token) >> f

throwApiError :: String -> Api a
throwApiError = throwError . GenericError

type URL = String
type Path = String
type Params = [(ByteString, Maybe ByteString)]

getJSON :: FromJSON a => URL -> Params -> Api a
getJSON url params = requestJSON url $ setQueryString params

getSource :: URL -> Params -> DownloadSink a -> Api a
getSource url params withSource = do
    request <- setQueryString params <$> authorize url

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
    token <- fmap apiToken ask
    request <- parseUrl' url

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

allowStatus :: Status -> Request -> Request
allowStatus status request =
    let original = checkStatus request
        override s r c
            | s == status = Nothing
            | otherwise = original s r c

    in request { checkStatus = override }

decodeBody :: FromJSON a => Response BL.ByteString -> Api a
decodeBody =
    either (throwError . InvalidJSON) return . eitherDecode . responseBody

debugApi :: String -> Api ()
debugApi msg = do
    debug <- fmap apiDebug ask

    when debug $ liftIO $ hPutStrLn stderr $ "[DEBUG]: " <> msg

parseUrl' :: URL -> Api Request
parseUrl' url = case parseUrl url of
    Just request -> return request
    Nothing -> throwApiError $ "Invalid URL: " <> url

tryHttp :: IO a -> Api a
tryHttp = either (throwError . HttpError) return <=< liftIO . E.try
