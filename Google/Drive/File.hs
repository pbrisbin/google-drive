-- |
--
-- Methods for working with File resources on Google Drive.
--
-- https://developers.google.com/drive/v2/reference/files
--
-- See @"Network.Google.Drive.Upload"@ for creating and updating file resources.
--
module Network.Google.Drive.File
    ( File(..)
    , FileId
    , Query(..)
    , Items(..)
    , getFiles
    , createFolder
    , downloadFile
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
import System.Directory (createDirectoryIfMissing)
import System.FilePath (takeDirectory)

import qualified Data.ByteString as B
import qualified Data.Text as T

import Network.Google.Api

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

getFiles :: Query -> Api [File]
getFiles query = do
    Items items <- getJSON (baseUrl <> "/files")
        [ ("q", Just $ toParam query)
        , ("maxResults", Just "1000")
        ]

    return $ filter (not . fileTrashed) items

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

toParam :: Query -> ByteString
toParam (TitleEq title) = "title = " <> quote title
toParam (ParentEq fileId) = quote fileId <> " in parents"
toParam (p `And` q) = "(" <> toParam p <> ") and (" <> toParam q <> ")"
toParam (p `Or` q) = "(" <> toParam p <> ") or (" <> toParam q <> ")"

quote :: Text -> ByteString
quote = ("'" <>) . (<> "'") . encodeUtf8
