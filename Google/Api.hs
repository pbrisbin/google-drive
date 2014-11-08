-- |
--
-- Actions for working with any of Google's APIs
--
module Network.Google.Api
    ( Api
    , ApiOptions(..)
    , ApiError(..)
    , UploadType(..)
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
    , downloadSink
    , uploadSource
    , askUploadType
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

import Control.Monad.Error
import Control.Monad.Reader
import Control.Monad.Trans.Resource
import Data.Aeson (FromJSON(..), ToJSON(..), eitherDecode, encode)
import Data.ByteString (ByteString)
import Data.Conduit
import Data.Conduit.Progress
import Data.Conduit.Throttle
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
import qualified Data.ByteString as B
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

data UploadType = Multipart | Resumable

data ApiOptions = ApiOptions
    { apiUploadType :: UploadType
    , apiThrottle :: Maybe Int -- ^ Throttle to x bytes per second
    , apiProgress :: Maybe Int -- ^ Show progress every x bytes
    , apiDebug :: Bool         -- ^ Unused currently
    }

instance Default ApiOptions where
    def = ApiOptions
        { apiUploadType = Resumable
        , apiThrottle = Nothing
        , apiProgress = Nothing
        , apiDebug = False
        }

data ApiConfig = ApiConfig
    { apiToken :: String
    , apiOptions :: ApiOptions
    }

type Api a = ReaderT ApiConfig (ErrorT ApiError IO) a

runApi :: String -> ApiOptions -> Api a -> IO (Either ApiError a)
runApi token options f = runErrorT $ runReaderT action $ ApiConfig token options
  where
    action = debugApi ("Using token: " <> token) >> f

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
    token <- fmap apiToken ask
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

allowStatus :: Status -> Request -> Request
allowStatus status request =
    let original = checkStatus request
        override s r c
            | s == status = Nothing
            | otherwise = original s r c

    in request { checkStatus = override }

askUploadType :: Api UploadType
askUploadType = fmap (apiUploadType . apiOptions) ask

-- | Convert the given sink to one with throttling and/or progress reporting
--   depending on the current @ApiOptions@
downloadSink :: MonadIO m
             => Maybe Int
             -> Sink ByteString m r
             -> Api (Sink ByteString m r)
downloadSink msize sink = do
    options <- fmap apiOptions ask

    return $ case (msize, options) of
        (Just s, (ApiOptions _ (Just l) (Just p) _)) ->
            throttled l =$ progress p s =$ sink
        (Just s, (ApiOptions _ _ (Just p) _)) -> progress p s =$ sink
        (_, (ApiOptions _ (Just l) _ _)) -> throttled l =$ sink
        _ -> sink

-- | Convert the given source to one with throttling and/or progress reporting
--   depending on the current @ApiOptions@
uploadSource :: MonadIO m
             => Maybe Int
             -> Source m ByteString
             -> Api (Source m ByteString)
uploadSource msize source = do
    options <- fmap apiOptions ask

    return $ case (msize, options) of
        (Just s, (ApiOptions _ (Just l) (Just p) _)) ->
            source $= throttled l $= progress p s
        (Just s, (ApiOptions _ _ (Just p) _)) -> source $= progress p s
        (_, (ApiOptions _ (Just l) _ _)) -> source $= throttled l
        _ -> source

decodeBody :: FromJSON a => Response BL.ByteString -> Api a
decodeBody response =
    case eitherDecode $ responseBody response of
        Left ex -> throwError $ InvalidJSON ex
        Right x -> return x

debugApi :: String -> Api ()
debugApi msg = do
    debug <- fmap (apiDebug . apiOptions) ask

    when debug $ liftIO $ hPutStrLn stderr $ "[DEBUG]: " <> msg

throttled :: MonadIO m => Int -> Conduit ByteString m ByteString
throttled limit = throttle B.length limit

progress :: MonadIO m => Int -> Int -> Conduit ByteString m ByteString
progress each size = reportProgress B.length size each

tryHttp :: IO a -> Api a
tryHttp f = do
    result <- liftIO $ E.try f

    case result of
        Left ex -> throwError $ HttpError ex
        Right x -> return x
