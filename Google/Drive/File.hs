-- |
--
-- Methods for working with File resources on Google Drive. For searching see,
-- @"Network.Google.Drive.Search"@. For uploading, see
-- @"Network.Google.Drive.Upload"@.
--
-- https://developers.google.com/drive/v2/reference/files
--
module Network.Google.Drive.File
    ( FileId
    , Items(..)
    , File(..)
    , getFile
    , createFolder
    , createFile
    , updateFile
    , downloadFile
    ) where

import Control.Applicative ((<$>), (<*>))
import Control.Monad (mzero)
import Data.Aeson (FromJSON(..), Value(..), (.=), (.:), (.:?), object)
import Data.Maybe (listToMaybe)
import Data.Monoid ((<>))
import Data.Text (Text)
import Data.Time (UTCTime)
import System.Directory (getModificationTime)
import System.FilePath (takeFileName)

import qualified Data.Text as T

import Network.Google.Drive.Api
import Network.Google.Drive.Upload

type FileId = Text

newtype Items = Items [File]

instance FromJSON Items where
    parseJSON (Object o) = Items
        <$> (mapM parseJSON =<< o .: "items")

    parseJSON _ = mzero

data File = File
    { fileId :: FileId
    , fileTitle :: Text
    , fileModified :: UTCTime
    -- | N.B. files with multiple parents are unsupported
    , fileParent :: Maybe FileId
    , fileTrashed :: Bool
    , fileSize :: Maybe Int
    , fileDownloadUrl :: Maybe Text
    }

instance Eq File where
    a == b = fileId a == fileId b

instance Show File where
    show File{..} = T.unpack $ fileTitle <> " (" <> fileId <> ")"

instance FromJSON File where
    parseJSON (Object o) = File
        <$> o .: "id"
        <*> o .: "title"
        <*> o .: "modifiedDate"
        <*> (listToMaybe <$> (mapM (.: "id") =<< o .: "parents"))
        <*> ((.: "trashed") =<< o .: "labels")
        <*> o .:? "fileSize"
        <*> o .:? "downloadUrl"

    parseJSON _ = mzero

getFile :: FileId -> Api (Maybe File)
getFile fileId = simpleApi $ "/files/" <> T.unpack fileId

createFolder :: FileId -- ^ Parent under which to create the folder
             -> Text   -- ^ Name of the folder
             -> Api (Maybe File)
createFolder parentId name = do
    logApi $ "CREATE FOLDER " <> T.unpack parentId <> "/" <> T.unpack name

    postApi "/files" $ object
        [ "title" .= name
        , "parents" .= [object ["id" .= parentId]]
        , "mimeType" .= folderType
        ]

  where
    folderType :: Text
    folderType = "application/vnd.google-apps.folder"

createFile :: FilePath -- ^ File to upload
           -> File     -- ^ Parent under which to create the file
           -> Api (Maybe File)
createFile path parent = do
    logApi $ "CREATE " <> path <> " --> " <> show parent

    localModified <- liftIO $ getModificationTime path

    let name = takeFileName path
        body = object
            [ "title" .= name
            , "parents" .= [object ["id" .= fileId parent]]
            , "modifiedDate" .= localModified
            ]

    postUpload "/files" body path

updateFile :: FilePath -- ^ File to upload from
           -> File     -- ^ Parent under which to create the file
           -> Api ()
updateFile path file = do
    logApi $ "UPDATE " <> path <> " --> " <> show file

    localModified <- liftIO $ getModificationTime path

    let body = object ["modifiedDate" .= localModified]
        apiPath = "/files/" <> (T.unpack $ fileId file)

    putUpload apiPath body path

downloadFile :: File -> FilePath -> Api ()
downloadFile file path = case fileDownloadUrl file of
    Nothing -> logApiErr $ show file <> " has no Download URL"
    Just url -> do
        logApi $ "DOWNLOAD " <> show file <> " --> " <> path
        authenticatedDownload (T.unpack url) path
