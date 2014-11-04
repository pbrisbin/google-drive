{-# LANGUAGE BangPatterns #-}
-- |
--
-- Uploads using the multipart method. Resumable uploads is a (far-off) TODO.
--
module Network.Google.Drive.Upload
    ( postUpload
    , putUpload
    ) where

import Control.Monad (void)
import Control.Monad.IO.Class
import Data.Aeson
import Data.Conduit
import Data.Conduit.Binary (sourceLbs)
import Data.ByteString.Lazy (ByteString)
import Data.Monoid ((<>))
import Network.HTTP.Client.MultipartFormData (webkitBoundary)
import Network.HTTP.Conduit
import Network.HTTP.Types
import Network.Mime
import System.IO (hFlush, stdout)

import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Char8 as C8
import qualified Data.Text as T

import Network.Google.Drive.Api

uploadUrl :: URL
uploadUrl = "https://www.googleapis.com/upload/drive/v2"

postUpload :: (ToJSON a, FromJSON b) => Path -> a -> FilePath -> Api (Maybe b)
postUpload path body filePath =
    fmap (decode . responseBody) $ uploadApi "POST" path body filePath

putUpload :: ToJSON a => Path -> a -> FilePath -> Api ()
putUpload path body filePath = void $ uploadApi "PUT" path body filePath

uploadApi :: ToJSON a
          => Method
          -> Path
          -> a
          -> FilePath
          -> Api (Response ByteString)
uploadApi method path body filePath = do
    request <- addMultipart body filePath =<< authorize =<< apiRequest path

    liftIO $ withManager $ httpLbs $ setMethod method request

addMultipart :: ToJSON a => a -> FilePath -> Request -> Api Request
addMultipart body filePath request = do
    boundary <- liftIO $ webkitBoundary
    fileContents <- liftIO $ BL.readFile filePath

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
            sourceLbs requestBody $= progress 1 (report requestLength)

    return $ modify request { requestBody = requestSource }

apiRequest :: Path -> Api Request
apiRequest path = liftIO $ parseUrl $ uploadUrl <> path

uploadQuery :: Params
uploadQuery =
    [ ("uploadType", Just "multipart")
    , ("setModifiedDate", Just "1")
    ]

mimeType :: FilePath -> MimeType
mimeType = mimeByExt defaultMimeMap defaultMimeType . T.pack

toLazy :: B.ByteString -> ByteString
toLazy b = C8.fromChunks [b]

-- Progress reporting. TODO: generalize, extract?

progress :: (MonadIO m) => Int -> (Int -> IO ()) -> Conduit i m i
progress n act = skipN n 1
  where
    skipN !c !t = do
        mv <- await

        case mv of
            Nothing -> return ()
            Just !v ->
               if (c :: Int) <= 1
                  then do
                     liftIO $ act t
                     yield v
                     skipN n (succ t)
                  else do
                     yield v
                     skipN (pred c) (succ t)

report :: (Show a, Show b) => a -> b -> IO ()
report total n = do
    putStr $ "  " <> format n total <> "\r"
    hFlush stdout

format :: (Show a, Show b) => a -> b -> String
format n t = "  " <> pad maxLen (show n) <> "/" <> show t <> "\r"
  where
    maxLen = length $ show t
    pad l v = replicate (l - length v) ' ' <> v
