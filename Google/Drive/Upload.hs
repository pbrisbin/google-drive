module Network.Google.Drive.Upload
    ( UploadType(..)
    , createFile
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

data UploadType = Multipart | Resumable

-- | Upload the local file under the given parent folder
createFile :: UploadType -> File -> FilePath -> Api File
createFile uploadType parent filePath = do
    localModified <- liftIO $ getModificationTime filePath

    let name = takeFileName filePath
        body = object
            [ "title" .= name
            , "parents" .= [object ["id" .= fileId parent]]
            , "modifiedDate" .= localModified
            ]

    postUpload uploadType "/files" body filePath

-- | Upload the local file, updating an existing remote file
updateFile :: UploadType -> File -> FilePath -> Api File
updateFile uploadType file filePath = do
    localModified <- liftIO $ getModificationTime filePath

    let path = "/files/" <> T.unpack (fileId file)
        body = object ["modifiedDate" .= localModified]

    putUpload uploadType path body filePath

postUpload :: ToJSON a => UploadType -> Path -> a -> FilePath -> Api File
postUpload Multipart = M.postUpload
postUpload Resumable = R.postUpload

putUpload :: ToJSON a => UploadType -> Path -> a -> FilePath -> Api File
putUpload Multipart = M.putUpload
putUpload Resumable = R.putUpload
