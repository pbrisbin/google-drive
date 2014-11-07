module Network.Google.Drive.Upload.Multipart
    ( postUpload
    , putUpload
    ) where

import Data.Aeson
import Data.Conduit
import Data.Conduit.Binary (sourceHandle, sourceLbs)
import Data.Conduit.Progress
import Data.ByteString.Lazy (ByteString)
import Data.Monoid ((<>))
import Network.HTTP.Client.MultipartFormData (webkitBoundary)
import Network.HTTP.Conduit
import Network.HTTP.Types
import Network.Mime
import System.IO

import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Char8 as C8
import qualified Data.Text as T

import Network.Google.Api

baseUrl :: URL
baseUrl = "https://www.googleapis.com/upload/drive/v2"

postUpload :: (ToJSON a, FromJSON b) => Path -> a -> FilePath -> Api b
postUpload path body filePath = do
    response <- requestIO (baseUrl <> path) $
        uploadMultipart body filePath . setMethod "POST"

    decodeBody response

putUpload :: (ToJSON a, FromJSON b) => Path -> a -> FilePath -> Api b
putUpload path body filePath = do
    response <- requestIO (baseUrl <> path) $
        uploadMultipart body filePath . setMethod "PUT"

    decodeBody response

uploadMultipart :: ToJSON a
                => a
                -> FilePath
                -> Request
                -> IO (Response ByteString)
uploadMultipart body filePath request = do
    boundary <- webkitBoundary

    withFile filePath ReadMode $ \h -> do
        fileSize <- fmap fromIntegral $ hFileSize h

        let before = C8.unlines
                [ "--" <> toLazy boundary
                , "Content-Type: application/json; charset=UTF-8"
                , ""
                , encode body
                , ""
                , "--" <> toLazy boundary
                , "Content-Type: " <> toLazy (mimeType filePath)
                , ""
                ]

            after = "\n--" <> toLazy boundary <> "--\n"

            source = do
                sourceLbs before
                sourceHandle h
                sourceLbs after

        let contentType = "multipart/related; boundary=\"" <> boundary <> "\""
            requestLength = BL.length before + BL.length after + fileSize

            modify =
                addHeader (hContentType, contentType) .
                setQueryString uploadQuery

            progress = reportProgress B.length (fromIntegral requestLength) 100

        withManager $ httpLbs $ modify request
            { requestBody =
                requestBodySource (fromIntegral requestLength) $
                source $= progress
            }

uploadQuery :: Params
uploadQuery =
    [ ("uploadType", Just "multipart")
    , ("setModifiedDate", Just "1")
    ]

mimeType :: FilePath -> MimeType
mimeType = mimeByExt defaultMimeMap defaultMimeType . T.pack

toLazy :: B.ByteString -> ByteString
toLazy b = C8.fromChunks [b]
