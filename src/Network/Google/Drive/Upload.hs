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
    , uploadFile
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
    ( Status
    , hContentLength
    , hContentType
    , hLocation
    , hRange
    , mkStatus
    , statusIsServerError
    )
import System.Random (randomRIO)

import qualified Data.ByteString.Char8 as C8

import Network.Google.Api
import Network.Google.Drive.File

-- | Uploads use sources for space efficiency and so that callers can implement
--   things like throttling or progress output themselves. Since uploads are
--   resumable, each invocation will give your @UploadSource@ the bytes
--   completed so far, so you may create an appropriately offset source (i.e.
--   into a file).
type UploadSource = Int -> Source (ResourceT IO) ByteString

-- | Simple @UploadSource@ for uploading from a file
uploadSourceFile :: FilePath -> UploadSource
uploadSourceFile fp 0 = sourceFile fp
uploadSourceFile fp c = sourceFileRange fp (Just $ fromIntegral $ c + 1) Nothing

baseUrl :: URL
baseUrl = "https://www.googleapis.com/upload/drive/v2"

uploadFile :: File -- ^ New or existing @File@
           -> Int  -- ^ Length of source
           -> UploadSource
           -> Api File
uploadFile file fileLength mkSource =
    withSessionUrl file $ \url ->
        retryWithBackoff 1 $ do
            completed <- getUploadedBytes url
            resumeUpload url completed fileLength mkSource

withSessionUrl :: File -> (URL -> Api b) -> Api b
withSessionUrl file action  = do
    response <- requestLbs (baseUrl <> uploadPath file) $
        setMethod (uploadMethod file) .
        setQueryString uploadQuery .
        setBody (encode $ uploadData file) .
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
getUploadedBytes url = do
    response <- requestLbs url $
        setMethod "PUT" .
        addHeader (hContentLength, "0") .
        addHeader ("Content-Range", "bytes */*") .
        allowStatus status308

    return $ fromMaybe 0 $ rangeEnd =<< lookup hRange (responseHeaders response)

  where
    rangeEnd :: ByteString -> Maybe Int
    rangeEnd = fmap read . stripPrefix "0-" . C8.unpack

resumeUpload :: URL -> Int -> Int -> UploadSource -> Api File
resumeUpload url completed fileLength mkSource = do
    let left = fileLength - completed

    requestJSON url $
        setMethod "PUT" .
        addRange completed .
        setBodySource (fromIntegral left) (mkSource completed)

  where
    addRange 0 = id
    addRange c = addHeader ("Content-Range", nextRange c fileLength)

    -- e.g. Content-Range: bytes 43-1999999/2000000
    nextRange c t = C8.pack $
        "bytes " <> show (c + 1) <> "-" <> show (t - 1) <> "/" <> show t

retryWithBackoff :: Int -> Api a -> Api a
retryWithBackoff seconds f = f `catchError` \e ->
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
