module Network.Google.Drive.Upload.Multipart
    ( postUpload
    , putUpload
    ) where

import Data.Aeson
import Data.Conduit
import Data.Conduit.Binary (sourceFile, sourceLbs)
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
import Network.Google.Drive.File

baseUrl :: URL
baseUrl = "https://www.googleapis.com/upload/drive/v2"

postUpload :: ToJSON a => Path -> a -> FilePath -> Api File
postUpload path body filePath = do
    modify <- liftIO $ addMultipart body filePath

    requestJSON (baseUrl <> path) $ modify . setMethod "POST"

putUpload :: ToJSON a => Path -> a -> FilePath -> Api File
putUpload path body filePath = do
    modify <- liftIO $ addMultipart body filePath

    requestJSON (baseUrl <> path) $ modify . setMethod "PUT"

addMultipart :: ToJSON a => a -> FilePath -> IO (Request -> Request)
addMultipart body filePath = do
    boundary <- webkitBoundary
    fileLength <- fmap fromIntegral $ withFile filePath ReadMode hFileSize

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
            sourceFile filePath
            sourceLbs after

        progress = reportProgress B.length (fromIntegral requestLength) 100
        contentType = "multipart/related; boundary=\"" <> boundary <> "\""
        requestLength = BL.length before + BL.length after + fileLength

    return $
        addHeader (hContentType, contentType) .
        setQueryString uploadQuery .
        setBodySource requestLength (source $= progress)

uploadQuery :: Params
uploadQuery =
    [ ("uploadType", Just "multipart")
    , ("setModifiedDate", Just "true")
    ]

mimeType :: FilePath -> MimeType
mimeType = mimeByExt defaultMimeMap defaultMimeType . T.pack

toLazy :: B.ByteString -> ByteString
toLazy b = C8.fromChunks [b]
