-- |
--
-- Methods for working with File resources on Google Drive.
--
-- https://developers.google.com/drive/v2/reference/files
--
module Network.Google.Drive.File
    (
    -- * File resources
      File(..)
    , FileId

    -- * Create, Update, Download
    , createFile
    , createFolder
    , updateFile
    , downloadFile

    -- * Searching
    , Query(..)
    , Items(..)
    , getFiles
    ) where

import Control.Applicative ((<$>), (<*>))
import Control.Monad (mzero)
import Data.Aeson (FromJSON(..), Value(..), (.=), (.:), (.:?), object)
import Data.ByteString (ByteString)
import Data.Conduit
import Data.Conduit.Binary (sinkFile)
import Data.Conduit.Progress
import Data.Maybe (listToMaybe)
import Data.Monoid ((<>))
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8)
import Data.Time (UTCTime)
import System.Directory (createDirectoryIfMissing, getModificationTime)
import System.FilePath (takeDirectory, takeFileName)

import qualified Data.ByteString as B
import qualified Data.Text as T

import Network.Google.Api
import Network.Google.Drive.Upload

type FileId = Text

data File = File
    { fileId :: !FileId
    , fileTitle :: !Text
    , fileModified :: !UTCTime
    -- | N.B. files with multiple parents are unsupported
    , fileParent :: !(Maybe FileId)
    , fileTrashed :: !Bool
    , fileSize :: !(Maybe Int)
    , fileDownloadUrl :: !(Maybe Text)
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
        <*> (fmap read <$> o .:? "fileSize") -- fileSize is a String!
        <*> o .:? "downloadUrl"

    parseJSON _ = mzero

data Query
    = TitleEq Text
    | ParentEq FileId
    | Query `And` Query
    | Query `Or` Query

newtype Items = Items [File]

instance FromJSON Items where
    parseJSON (Object o) = Items
        <$> (mapM parseJSON =<< o .: "items")

    parseJSON _ = mzero

baseUrl :: URL
baseUrl = "https://www.googleapis.com/drive/v2"

createFolder :: FileId -- ^ Parent under which to create the folder
             -> Text   -- ^ Name of the folder
             -> Api File
createFolder parentId name = do
    postJSON (baseUrl <> "/files") [] $ object
        [ "title" .= name
        , "parents" .= [object ["id" .= parentId]]
        , "mimeType" .= folderType
        ]

  where
    folderType :: Text
    folderType = "application/vnd.google-apps.folder"

createFile :: FilePath -- ^ File to upload
           -> File     -- ^ Parent under which to create the file
           -> Api File
createFile path parent = do
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
           -> Api File
updateFile path file = do
    localModified <- liftIO $ getModificationTime path

    let body = object ["modifiedDate" .= localModified]
        apiPath = "/files/" <> (T.unpack $ fileId file)

    putUpload apiPath body path

downloadFile :: File -> FilePath -> Api ()
downloadFile file filePath =
    case fmap T.unpack $ fileDownloadUrl file of
        Nothing -> throwApiError $ show file <> " has no Download URL"
        Just url -> getSource url [] $ \source -> do
            liftIO $ createDirectoryIfMissing True $ takeDirectory filePath

            let progress size = reportProgress B.length size 100

            -- We can only report progress if we know final size
            source $$+- case (fileSize file) of
                Nothing -> sinkFile filePath
                Just size -> progress size =$ sinkFile filePath

getFiles :: Query -> Api [File]
getFiles query = do
    Items items <- getJSON (baseUrl <> "/files")
        [ ("q", Just $ toParam query)
        , ("maxResults", Just "1000")
        ]

    return $ filter (not . fileTrashed) items

toParam :: Query -> ByteString
toParam (TitleEq title) = "title = " <> quote title
toParam (ParentEq fileId) = quote fileId <> " in parents"
toParam (p `And` q) = "(" <> toParam p <> ") and (" <> toParam q <> ")"
toParam (p `Or` q) = "(" <> toParam p <> ") or (" <> toParam q <> ")"

quote :: Text -> ByteString
quote = ("'" <>) . (<> "'") . encodeUtf8
