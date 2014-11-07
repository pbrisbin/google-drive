module Network.Google.Drive.Upload.Multipart
    ( postUpload
    , putUpload
    ) where

import Data.Aeson
import Data.Conduit
import Data.Conduit.Binary (sourceLbs)
import Data.Conduit.Progress
import Data.ByteString.Lazy (ByteString)
import Data.Monoid ((<>))
import Network.HTTP.Client.MultipartFormData (webkitBoundary)
import Network.HTTP.Conduit
import Network.HTTP.Types
import Network.Mime

import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Char8 as C8
import qualified Data.Text as T

import Network.Google.Api

baseUrl :: URL
baseUrl = "https://www.googleapis.com/upload/drive/v2"

postUpload :: (ToJSON a, FromJSON b) => Path -> a -> FilePath -> Api b
postUpload path body filePath =
    requestJSON (baseUrl <> path) $
    addMultipart body filePath . setMethod "POST"

putUpload :: (ToJSON a, FromJSON b) => Path -> a -> FilePath -> Api b
putUpload path body filePath =
    requestJSON (baseUrl <> path) $
    addMultipart body filePath . setMethod "PUT"

addMultipart :: ToJSON a => a -> FilePath -> Request -> IO Request
addMultipart body filePath request = do
    boundary <- webkitBoundary
    fileContents <- BL.readFile filePath

    let contentType = "multipart/related; boundary=\"" <> boundary <> "\""
        requestBody = C8.unlines
            [ "--" <> toLazy boundary
            , "Content-Type: application/json; charset=UTF-8"
            , ""
            , encode body
            , ""
            , "--" <> toLazy boundary
            , "Content-Type: " <> toLazy (mimeType filePath)
            , ""
            , fileContents
            , "--" <> toLazy boundary <> "--"
            ]
        requestLength = BL.length requestBody

        modify =
            addHeader (hContentType, contentType) .
            setQueryString uploadQuery

        requestSource = requestBodySource requestLength $
            sourceLbs requestBody $=
            reportProgress B.length (fromIntegral requestLength) 100

    return $ modify request { requestBody = requestSource }

uploadQuery :: Params
uploadQuery =
    [ ("uploadType", Just "multipart")
    , ("setModifiedDate", Just "1")
    ]

mimeType :: FilePath -> MimeType
mimeType = mimeByExt defaultMimeMap defaultMimeType . T.pack

toLazy :: B.ByteString -> ByteString
toLazy b = C8.fromChunks [b]
