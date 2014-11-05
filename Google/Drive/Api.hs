-- |
--
-- Low-level interface to the Google Drive API
--
-- Example:
--
-- > import Network.Google.Drive.Api
-- > import Network.Google.Drive.Search (getFiles)
-- >
-- > main :: IO ()
-- > main = do
-- >     token <- getAccessToken -- however you do that
-- >
-- >     runApi token $ do
-- >         files <- getFiles $ TitleEq "foo.txt"
-- >
-- >         case files of
-- >             (file:_) ->
-- >                 case fileDownloadUrl file of
-- >                     Just url -> authenticatedDownload url "file.txt"
-- >                     Nothing -> return ()
-- >             _ -> return ()
--
module Network.Google.Drive.Api
    (
    -- * The Api monad
      Api
    , runApi

    -- * Types
    , Path
    , Params
    , URL

    -- * Api actions
    , getApi
    , postApi

    -- * Logging
    -- |
    --
    -- Logging is implemented by printing strings to stdout or stderr. This will
    -- eventually be replaced by something more structured.
    --
    , logApi
    , logApiErr

    -- * Utilities
    , simpleApi
    , authenticatedDownload
    , authorize
    , addHeader
    , setBody
    , setMethod

    -- * Re-exports
    , liftIO
    ) where

import Control.Monad.Reader
import Data.Aeson (FromJSON(..), ToJSON(..), decode, encode)
import Data.ByteString (ByteString)
import Data.Conduit
import Data.Conduit.Binary (sinkFile)
import Data.Monoid ((<>))
import Network.HTTP.Conduit
    ( Request(..)
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
import System.IO (hPutStrLn, stderr)

import qualified Data.ByteString.Char8 as C8
import qualified Data.ByteString.Lazy as BL

type Api a = ReaderT String IO a

runApi :: String -> Api a -> IO a
runApi token f = runReaderT f token

-- | Prints the string to stdout (temporary)
logApi :: String -> Api ()
logApi = liftIO . putStrLn

-- | Prints the string to stderr (temporary)
logApiErr :: String -> Api ()
logApiErr = liftIO . hPutStrLn stderr

type URL = String
type Path = String
type Params = [(ByteString, Maybe ByteString)]

baseUrl :: URL
baseUrl = "https://www.googleapis.com/drive/v2"

-- | Perform a GET request to the given path, decoding the response as JSON
simpleApi :: FromJSON a => Path -> Api (Maybe a)
simpleApi path = getApi path []

getApi :: FromJSON a => Path -> Params -> Api (Maybe a)
getApi path query = do
    request <- fmap (setQueryString query) $ authorize =<< apiRequest path

    fmap (decode . responseBody) $ withManager $ httpLbs request

postApi :: (ToJSON a, FromJSON b) => Path -> a -> Api (Maybe b)
postApi path body = do
    let content = (hContentType, "application/json")
        modify = addHeader content . setBody (encode body) . setMethod "POST"

    request <- fmap modify $ authorize =<< apiRequest path

    fmap (decode . responseBody) $ withManager $ httpLbs request

-- TODO: incorporate progress
authenticatedDownload :: URL -> FilePath -> Api ()
authenticatedDownload url path = do
    request <- authorize =<< (liftIO $ parseUrl url)

    liftIO $ createDirectoryIfMissing True $ takeDirectory path

    withManager $ \manager -> do
        response <- http request manager
        responseBody response $$+- sinkFile path

apiRequest :: Path -> Api Request
apiRequest path = liftIO $ parseUrl $ baseUrl <> path

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
