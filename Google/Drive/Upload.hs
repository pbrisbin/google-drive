module Network.Google.Drive.Upload
    ( uploadFile
    , uploadNewFile
    ) where

import Control.Concurrent (threadDelay)
import Control.Monad.Trans.Resource (ResourceT)
import Data.Aeson
import Data.ByteString (ByteString)
import Data.Conduit
import Data.List (stripPrefix)
import Data.Maybe (fromMaybe)
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
import System.Random (randomRIO)

import qualified Data.ByteString.Char8 as C8
--import qualified Data.Conduit.Binary as CB
import qualified Data.Text as T

import Network.Google.Api
import Network.Google.Drive.File

baseUrl :: URL
baseUrl = "https://www.googleapis.com/upload/drive/v2"

uploadFile :: File -> Int -> UploadSource -> Api File
uploadFile (File fileId fileData) =
    resumableUpload "PUT" (T.unpack $ "/files/" <> fileId) fileData

uploadNewFile :: FileData -> Int -> UploadSource -> Api File
uploadNewFile = resumableUpload "/files" "POST"

resumableUpload :: Method -> Path -> FileData -> Int -> UploadSource -> Api File
resumableUpload method path fileData fileLength source =
    withSessionUrl method path fileData $ \sessionUrl -> do
        completed <- getUploadedBytes sessionUrl

        retryWithBackoff 1 $
            resumeUpload sessionUrl completed fileLength source

withSessionUrl :: Method -> Path -> FileData -> (URL -> Api b) -> Api b
withSessionUrl method path fileData action = do
    response <- requestLbs (baseUrl <> path) $
        setMethod method .
        setQueryString uploadQuery .
        setBody (encode fileData) .
        addHeader (hContentType, "application/json")

    case lookup hLocation $ responseHeaders response of
        Just url -> action $ C8.unpack url
        Nothing -> throwApiError "Resumable upload Location header missing"

  where
    uploadQuery :: Params
    uploadQuery =
        [ ("uploadType", Just "resumable")
        , ("setModifiedDate", Just "true")
        ]

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
    rangeEnd = fromMaybe 0 . fmap read . stripPrefix "0-" . C8.unpack

resumeUpload :: URL
             -> Int -- ^ completed
             -> Int -- ^ total
             -> Source (ResourceT IO) ByteString
             -> Api File
resumeUpload sessionUrl completed fileLength source = do
    let left = fileLength - completed
        range = nextRange completed fileLength

    debugApi $ C8.unpack $ "Resuming upload for range: " <> range

    -- TODO CB.drop completed $ source
    requestJSON sessionUrl $
        setMethod "PUT" .
        addHeader ("Content-Range", range) .
        setBodySource (fromIntegral left) source

  where
    -- e.g. Content-Range: bytes 43-1999999/2000000
    nextRange c t = C8.pack $
        "bytes " <> show (c + 1) <> "-" <> show (t - 1) <> "/" <> show t

retryWithBackoff :: Int -> Api a -> Api a
retryWithBackoff seconds f = f `catchError` \e -> do
    debugApi $ "Error: " <> show e
    debugApi $ "Current backoff: " <> show seconds <> " seconds"

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
