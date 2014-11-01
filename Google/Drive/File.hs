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

import qualified Data.Text as T

import Network.Google.Drive.Api

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
    , fileParent :: Maybe FileId
    , fileTrashed :: Bool
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
        <*> o .:? "downloadUrl"

    parseJSON _ = mzero

getFile :: FileId -> Api (Maybe File)
getFile fileId = simpleApi $ "/files/" <> T.unpack fileId

createFolder :: FileId -> Text -> Api (Maybe File)
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

-- TODO
createFile :: FilePath -> File -> Api FileId
createFile path parent = do
    logApi $ "CREATE " <> path <> " --> " <> show parent

    return "new"

-- TODO
updateFile :: FilePath -> File -> Api ()
updateFile path file =
    logApi $ "UPDATE " <> path <> " --> " <> show file

downloadFile :: File -> FilePath -> Api ()
downloadFile file path = case fileDownloadUrl file of
    Nothing -> logApi $ show file <> " had no Download URL"
    Just url -> do
        logApi $ "DOWNLOAD " <> show file <> " --> " <> path
        authenticatedDownload (T.unpack url) path
