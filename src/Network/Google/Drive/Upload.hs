{-# LANGUAGE OverloadedStrings #-}
-- |
--
-- Resumable uploads
--
-- https://developers.google.com/drive/web/manage-uploads#resumable
--
-- Note: actual resuming of uploads on errors is currently untested.
--
module Network.Google.Drive.Upload
    ( UploadSource
    , uploadSourceFile
    , createFileWithContent
    , updateFileWithContent
    ) where

import Control.Concurrent (threadDelay)
import Control.Monad.Trans.Resource (ResourceT)
import Data.Aeson
import Data.ByteString (ByteString)
import Data.Conduit
import Data.Conduit.Binary (sourceFile, sourceFileRange)
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
import qualified Data.Text as T

import Network.Google.Api
import Network.Google.Drive.File

-- | Uploads use sources for space efficiency and so that callers can implement
--   things like throttling or progress output themselves. Since uploads are
--   resumable, each invocation will give your @UploadSource@ the bytes
--   completed so far, so you may create an appropriately offset source (i.e.
--   into a file).
type UploadSource = Int -> Source (ResourceT IO) ByteString

type URL = String

-- | Simple @UploadSource@ for uploading from a file
uploadSourceFile :: FilePath -> UploadSource
uploadSourceFile fp 0 = sourceFile fp
uploadSourceFile fp c = sourceFileRange fp (Just $ fromIntegral $ c + 1) Nothing

createFileWithContent :: FileData -> Int -> UploadSource -> DriveApi File
createFileWithContent = uploadContent "POST" "/upload/drive/v2/files"

updateFileWithContent :: FileId -> FileData -> Int -> UploadSource -> DriveApi File
updateFileWithContent fid = uploadContent "PUT"
    (Path $ "/upload/drive/v2/files/" <> T.unpack fid)

uploadContent :: Method -> Resource -> FileData -> Int -> UploadSource -> DriveApi File
uploadContent m r fd fl mkSource =
    withSessionUrl m r fd $ \url ->
        retryWithBackoff 1 $ do
            completed <- getUploadedBytes url
            resumeUpload url completed fl mkSource

withSessionUrl :: Method -> Resource -> FileData -> (URL -> DriveApi a) -> DriveApi a
withSessionUrl m r fd action  = do
    response <- requestLbs r $
        setMethod m .
        setQueryString uploadQuery .
        setBody (encode fd) .
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

getUploadedBytes :: URL -> DriveApi Int
getUploadedBytes url = do
    response <- requestLbs (URL url) $
        setMethod "PUT" .
        addHeader (hContentLength, "0") .
        addHeader ("Content-Range", "bytes */*") .
        allowStatus status308

    return $ fromMaybe 0 $ rangeEnd =<< lookup hRange (responseHeaders response)

  where
    rangeEnd :: ByteString -> Maybe Int
    rangeEnd = fmap read . stripPrefix "0-" . C8.unpack

resumeUpload :: URL -> Int -> Int -> UploadSource -> DriveApi File
resumeUpload url completed fileLength mkSource = do
    let left = fileLength - completed

    requestJSON (URL url) $
        setMethod "PUT" .
        addRange completed .
        setBodySource (fromIntegral left) (mkSource completed)

  where
    addRange 0 = id
    addRange c = addHeader ("Content-Range", nextRange c fileLength)

    -- e.g. Content-Range: bytes 43-1999999/2000000
    nextRange c t = C8.pack $
        "bytes " <> show (c + 1) <> "-" <> show (t - 1) <> "/" <> show t

retryWithBackoff :: Int -> DriveApi a -> DriveApi a
retryWithBackoff seconds f = f `catchError` \e ->
    if seconds < 16 && retryable e
        then delay >> retryWithBackoff (seconds * 2) f
        else throwError e

  where
    retryable :: ApiError -> Bool
    retryable (HttpError (StatusCodeException s _ _)) = statusIsServerError s
    retryable _ = False

    delay :: DriveApi ()
    delay = liftIO $ do
        ms <- randomRIO (0, 999)
        threadDelay $ (seconds * 1000 + ms) * 1000

status308 :: Status
status308 = mkStatus 308 "Resume Incomplete"

setBodySource :: Int -> Source (ResourceT IO) ByteString -> Request -> Request
setBodySource = undefined
