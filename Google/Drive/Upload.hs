-- |
--
-- Delegating functions depending on the configured @'UploadType'@.
--
module Network.Google.Drive.Upload
    ( createFile
    , updateFile
    ) where

import Data.Aeson
import Data.Monoid ((<>))
import System.Directory (getModificationTime)
import System.FilePath (takeFileName)

import qualified Data.Text as T

import Network.Google.Api
import Network.Google.Drive.File

import qualified Network.Google.Drive.Upload.Multipart as M
import qualified Network.Google.Drive.Upload.Resumable as R

-- | Upload the local file under the given parent folder
createFile :: File -> FilePath -> Api File
createFile parent filePath = do
    localModified <- liftIO $ getModificationTime filePath

    let name = takeFileName filePath
        body = object
            [ "title" .= name
            , "parents" .= [object ["id" .= fileId parent]]
            , "modifiedDate" .= localModified
            ]

    postUpload "/files" body filePath

-- | Upload the local file, updating an existing remote file
updateFile :: File -> FilePath -> Api File
updateFile file filePath = do
    localModified <- liftIO $ getModificationTime filePath

    let path = "/files/" <> T.unpack (fileId file)
        body = object ["modifiedDate" .= localModified]

    putUpload path body filePath

postUpload :: ToJSON a => Path -> a -> FilePath -> Api File
postUpload path body filePath = do
    uploadType <- askUploadType
    case uploadType of
        Multipart -> M.postUpload path body filePath
        Resumable -> R.postUpload path body filePath

putUpload :: ToJSON a => Path -> a -> FilePath -> Api File
putUpload path body filePath = do
    uploadType <- askUploadType
    case uploadType of
        Multipart -> M.putUpload path body filePath
        Resumable -> R.putUpload path body filePath
