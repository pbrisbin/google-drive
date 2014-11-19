{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}
-- |
--
-- Actions for working with any of Google's APIs
--
-- Note: this module may become a standalone package at some point.
--
module Network.Google.Api
    (
    -- * The Api monad
      Api
    , ApiError(..)
    , runApi
    , runApi_
    , throwApiError

    -- * HTTP-related types
    , URL
    , Path
    , Params

    -- * High-level requests
    , DownloadSink
    , getJSON
    , getSource
    , postJSON

    -- * Lower-level requests
    , requestJSON
    , requestLbs

    -- * Api helpers
    , authorize
    , decodeBody

    -- * Request helpers
    , addHeader
    , allowStatus
    , setBody
    , setBodySource
    , setMethod

    -- * Re-exports
    , liftIO
    , throwError
    , catchError
    , sinkFile
    ) where

import Control.Applicative ((<$>))
import Control.Monad.Error
import Control.Monad.Reader
import Control.Monad.Trans.Resource
import Data.Aeson (FromJSON(..), ToJSON(..), eitherDecode, encode)
import Data.ByteString (ByteString)
import Data.Conduit
import Data.Conduit.Binary (sinkFile)
import Data.Monoid ((<>))
import Data.Typeable
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
import Network.HTTP.Types
    ( Header
    , Method
    , Status
    , hAuthorization
    , hContentType
    )

import qualified Control.Exception as E
import qualified Data.ByteString.Char8 as C8
import qualified Data.ByteString.Lazy as BL

-- | Downloads use sinks for space efficiency and so that callers can implement
--   things like throttling or progress output themselves. If you just want to
--   download to a file, use the re-exported @'sinkFile'@
type DownloadSink a =
    ResumableSource (ResourceT IO) ByteString -> ResourceT IO a

data ApiError
    = HttpError HttpException -- ^ Exceptions raised by http-conduit
    | InvalidJSON String      -- ^ Failure to parse a response as JSON
    | GenericError String     -- ^ All other errors
    deriving Typeable

instance Show ApiError where
    show (HttpError ex) = "HTTP Exception: " <> show ex
    show (InvalidJSON msg) = "failure parsing JSON: " <> msg
    show (GenericError msg) = msg

instance Error ApiError where
    strMsg = GenericError

instance E.Exception ApiError

-- | A transformer stack for providing the access token and rescuing errors
type Api = ReaderT String (ErrorT ApiError IO)

-- | Run an @Api@ computation with the given Access token
runApi :: String -- ^ OAuth2 access token
       -> Api a
       -> IO (Either ApiError a)
runApi token f = runErrorT $ runReaderT f token

-- | Like @runApi@ but discards the result and raises @ApiError@s as exceptions
runApi_ :: String -> Api a -> IO ()
runApi_ token f = either E.throw (const $ return ()) =<< runApi token f

-- | Abort an @Api@ computation with the given message
throwApiError :: String -> Api a
throwApiError = throwError . GenericError

type URL = String
type Path = String
type Params = [(ByteString, Maybe ByteString)]

-- | Make an authorized GET request for JSON
getJSON :: FromJSON a => URL -> Params -> Api a
getJSON url params = requestJSON url $ setQueryString params

-- | Make an authorized GET request, sending the response to the given sink
getSource :: URL -> Params -> DownloadSink a -> Api a
getSource url params withSource = do
    request <- setQueryString params <$> authorize url

    tryHttp $ withManager $ \manager -> do
        response <- http request manager
        withSource $ responseBody response

-- | Make an authorized POST request for JSON
postJSON :: (ToJSON a, FromJSON b) => URL -> Params -> a -> Api b
postJSON url params body =
    requestJSON url $
        addHeader (hContentType, "application/json") .
        setMethod "POST" .
        setQueryString params .
        setBody (encode body)

-- | Make an authorized request for JSON, first modifying it via the passed
--   function
requestJSON :: FromJSON a => URL -> (Request -> Request) -> Api a
requestJSON url modify = decodeBody =<< requestLbs url modify

-- | Make an authorized request, first modifying it via the passed function, and
--   returning the raw response content
requestLbs :: URL -> (Request -> Request) -> Api (Response BL.ByteString)
requestLbs url modify = do
    request <- authorize url

    tryHttp $ withManager $ httpLbs $ modify request

-- | Create an authorized request for the given URL
authorize :: URL -> Api Request
authorize url = do
    token <- ask
    request <- parseUrl' url

    let authorization = C8.pack $ "Bearer " <> token

    return $ addHeader (hAuthorization, authorization) request

addHeader :: Header -> Request -> Request
addHeader header request =
    request { requestHeaders = header:requestHeaders request }

setMethod :: Method -> Request -> Request
setMethod m request = request { method = m }

setBody :: BL.ByteString -> Request -> Request
setBody bs request = request { requestBody = RequestBodyLBS bs }

setBodySource :: Int64 -> Source (ResourceT IO) ByteString -> Request -> Request
setBodySource len source request =
    request { requestBody = requestBodySource len source }

-- | Modify the Request's status check to not treat the given status as an error
allowStatus :: Status -> Request -> Request
allowStatus status request =
    let original = checkStatus request
        override s r c
            | s == status = Nothing
            | otherwise = original s r c

    in request { checkStatus = override }

-- | Decode a JSON body, capturing failure as an @'ApiError'@
decodeBody :: FromJSON a => Response BL.ByteString -> Api a
decodeBody =
    either (throwError . InvalidJSON) return . eitherDecode . responseBody

parseUrl' :: URL -> Api Request
parseUrl' url = case parseUrl url of
    Just request -> return request
    Nothing -> throwApiError $ "Invalid URL: " <> url

tryHttp :: IO a -> Api a
tryHttp = either (throwError . HttpError) return <=< liftIO . E.try
