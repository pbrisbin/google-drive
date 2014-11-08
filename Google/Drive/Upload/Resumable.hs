module Network.Google.Drive.Upload.Resumable
    ( postUpload
    , putUpload
    ) where

import Control.Concurrent (threadDelay)
import Control.Monad (void)
import Data.Aeson
import Data.ByteString (ByteString)
import Data.Conduit.Binary (sourceFile, sourceFileRange)
import Data.List (stripPrefix)
import Data.Monoid ((<>))
import Network.HTTP.Conduit
import Network.HTTP.Types
    ( Method
    , Status
    , hContentLength
    , hContentType
    , hLocation
    , hRange
    , mkStatus
    , statusIsServerError
    )
import System.Directory (getTemporaryDirectory, removeFile)
import System.FilePath ((</>), (<.>), isPathSeparator)
import System.IO
import System.Random (randomRIO)

import qualified Control.Exception as E
import qualified Data.ByteString.Char8 as C8

import Network.Google.Api
import Network.Google.Drive.File

baseUrl :: URL
baseUrl = "https://www.googleapis.com/upload/drive/v2"

postUpload :: ToJSON a => Path -> a -> FilePath -> Api File
postUpload path body = retryWithBackoff 1 . resumableUpload "POST" path body

putUpload :: ToJSON a => Path -> a -> FilePath -> Api File
putUpload path body = retryWithBackoff 1 . resumableUpload "PUT" path body

resumableUpload :: ToJSON a => Method -> Path -> a -> FilePath -> Api File
resumableUpload method path body filePath = do
    msessionUrl <- liftIO $ cachedSessionUrl filePath

    debugApi $ "Cached session URL: " <> show msessionUrl

    cleaningUpCacheFile filePath $
        case msessionUrl of
            Nothing -> do
                sessionUrl <- initiateUpload method path body
                liftIO $ cacheSessionUrl filePath sessionUrl

                debugApi $ "Cached session URL: " <> show msessionUrl
                beginUpload sessionUrl filePath

            Just sessionUrl -> do
                range <- getUploadedBytes sessionUrl

                debugApi $ "Already uploaded: " <> show range
                resumeUpload sessionUrl range filePath

initiateUpload :: ToJSON a => Method -> Path -> a -> Api URL
initiateUpload method path body = do
    response <- requestLbs (baseUrl <> path) $
        setMethod method .
        setQueryString uploadQuery .
        setBody (encode body) .
        addHeader (hContentType, "application/json")

    case lookup hLocation $ responseHeaders response of
        Just url -> return $ C8.unpack url
        Nothing -> throwApiError "Resumable upload Location header missing"

  where
    uploadQuery :: Params
    uploadQuery =
        [ ("uploadType", Just "resumable")
        , ("setModifiedDate", Just "true")
        ]

beginUpload :: URL -> FilePath -> Api File
beginUpload sessionUrl filePath = do
    fileLength <- liftIO $ withFile filePath ReadMode hFileSize

    source <- uploadSource
        (Just $ fromIntegral fileLength) $ sourceFile filePath

    let modify =
            setMethod "PUT" .
            setBodySource (fromIntegral fileLength) source

    requestJSON sessionUrl modify

getUploadedBytes :: URL -> Api Int
getUploadedBytes sessionUrl = do
    response <- requestLbs sessionUrl $
        setMethod "PUT" .
        addHeader (hContentLength, "0") .
        addHeader ("Content-Range", "bytes */*") .
        allowStatus status308

    case lookup hRange $ responseHeaders response of
        Just range -> return $ rangeEnd range
        Nothing -> throwApiError "Resumable upload Range header missing"

  where
    rangeEnd :: ByteString -> Int
    rangeEnd b = case stripPrefix "0-" $ C8.unpack b of
        Just bs -> read bs
        Nothing -> 0

resumeUpload :: URL -> Int -> FilePath -> Api File
resumeUpload sessionUrl completed filePath = do
    fileLength <- liftIO $ withFile filePath ReadMode hFileSize

    let range = nextRange completed fileLength
        offset = fromIntegral $ completed + 1

    debugApi $ C8.unpack $ "Resuming upload for range: " <> range

    source <- uploadSource
        (Just $ fromIntegral fileLength) $
        sourceFileRange filePath (Just offset) Nothing

    let modify =
            setMethod "PUT" .
            addHeader ("Content-Range", range) .
            setBodySource (fromIntegral fileLength) source

    requestJSON sessionUrl modify

  where
    -- Content-Range: bytes 43-1999999/2000000
    nextRange c t = C8.pack $
        "bytes " <> show (c + 1) <> "-" <> show (t - 1) <> "/" <> show t

cachedSessionUrl :: FilePath -> IO (Maybe URL)
cachedSessionUrl filePath = do
    fp <- cacheFile filePath
    result <- try $ readFile fp

    return $ case result of
        Right url -> Just url
        _ -> Nothing

cacheSessionUrl :: FilePath -> URL -> IO ()
cacheSessionUrl filePath url = do
    fp <- cacheFile filePath

    void $ try $ writeFile fp url

cleaningUpCacheFile :: FilePath -> Api a -> Api a
cleaningUpCacheFile filePath action = do
    result <- action
    liftIO $ removeFile =<< cacheFile filePath
    debugApi $ "Removed session file for" <> filePath

    return result

cacheFile :: FilePath -> IO FilePath
cacheFile filePath = do
    tmp <- getTemporaryDirectory
    return $ tmp </> "drive-upload" <> sanitize filePath <.> "session"
  where
    sanitize = map (replacePathSeparator '-')
    replacePathSeparator c x
        | isPathSeparator x = c
        | otherwise = x

retryWithBackoff :: Int -> Api a -> Api a
retryWithBackoff seconds f = f `catchError` \e -> do
    debugApi $ "Error: " <> show e
    debugApi   "Possibly retrying..."

    if seconds < 16 && retryable e
        then delay >> retryWithBackoff (seconds * 2) f
        else throwError e

  where
    retryable :: ApiError -> Bool
    retryable (HttpError (StatusCodeException s _ _)) = statusIsServerError s
    retryable _ = False

    delay :: Api ()
    delay = liftIO $ do
        ms <- randomRIO (0, 999)
        threadDelay $ (seconds * 1000 + ms) * 1000

status308 :: Status
status308 = mkStatus 308 "Resume Incomplete"

try :: IO a -> IO (Either E.IOException a)
try = E.try
