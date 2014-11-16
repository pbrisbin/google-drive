-- |
--
-- Methods for working with File resources on Google Drive.
--
-- https://developers.google.com/drive/v2/reference/files
--
-- See also: @"Network.Google.Drive.Upload"@
--
module Network.Google.Drive.File
    ( File(..)
    , FileId
    , FileData(..)
    , fileId
    , fileData
    , uploadMethod
    , uploadPath
    , uploadData
    , Query(..)
    , Items(..)
    , getFile
    , insertFile
    , listFiles
    , newFolder
    , newFile
    ) where

import Control.Applicative ((<$>), (<*>))
import Control.Monad (mzero)
import Data.Aeson
import Data.ByteString (ByteString)
import Data.Monoid ((<>))
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8)
import Data.Time (UTCTime, getCurrentTime)
import Network.HTTP.Types (Method)
import System.Directory (getModificationTime)
import System.FilePath (takeFileName)

import qualified Data.Text as T

import Network.Google.Api

type FileId = Text

data FileData = FileData
    { fileTitle :: !Text
    , fileModified :: !UTCTime
    , fileParents :: ![FileId]
    , fileTrashed :: !Bool
    , fileSize :: !(Maybe Int)
    , fileDownloadUrl :: !(Maybe Text)
    , fileMimeType :: !Text
    }

data File = File FileId FileData | New FileData

instance Eq File where
    File a _ == File b _ = a == b
    _ == _ = False

instance Show File where
    show (File fi fd) = T.unpack $ fileTitle fd <> " (" <> fi <> ")"
    show (New fd) = show $ File "new" fd

fileId :: File -> FileId
fileId (File x _) = x
fileId _ = error "Cannot get fileId for new File"

fileData :: File -> FileData
fileData (File _ x) = x
fileData (New x) = x

uploadMethod :: File -> Method
uploadMethod (File _ _) = "PUT"
uploadMethod (New _) = "POST"

uploadPath :: File -> Path
uploadPath (File fid _) = "/files/" <> T.unpack fid
uploadPath (New _) = "/files"

uploadData :: File -> FileData
uploadData (File _ fd) = fd
uploadData (New fd) = fd

newtype Items = Items [File]

instance FromJSON FileData where
    parseJSON (Object o) = FileData
        <$> o .: "title"
        <*> o .: "modifiedDate"
        <*> (mapM (.: "id") =<< o .: "parents")
        <*> ((.: "trashed") =<< o .: "labels")
        <*> (fmap read <$> o .:? "fileSize") -- fileSize is a String!
        <*> o .:? "downloadUrl"
        <*> o .: "mimeType"

    parseJSON _ = mzero

instance FromJSON File where
    parseJSON v@(Object o) = File
        <$> o .: "id"
        <*> parseJSON v

    parseJSON _ = mzero

instance ToJSON FileData where
    toJSON FileData{..} = object
        [ "title" .= fileTitle
        , "modifiedDate" .= fileModified
        , "parents" .= object (map ("id" .=) $ fileParents)
        , "labels" .= object ["trashed" .= fileTrashed]
        , "mimeType" .= fileMimeType
        ]

instance FromJSON Items where
    parseJSON (Object o) = Items <$> (mapM parseJSON =<< o .: "items")
    parseJSON _ = mzero

data Query
    = TitleEq Text
    | ParentEq FileId
    | Untrashed
    | Query `And` Query
    | Query `Or` Query

baseUrl :: URL
baseUrl = "https://www.googleapis.com/drive/v2"

getFile :: FileId -> Api File
getFile fid = getJSON (baseUrl <> "/files/" <> T.unpack fid) []

insertFile :: File -> Api File
insertFile (New fd) = postJSON (baseUrl <> "/files") [] fd
insertFile _ = throwApiError "Cannot insert existing file"

listFiles :: Query -> Api [File]
listFiles query = do
    Items items <- getJSON (baseUrl <> "/files")
        [ ("q", Just $ toParam query)
        , ("maxResults", Just "1000")
        ]

    return $ items

  where
    toParam :: Query -> ByteString
    toParam (TitleEq title) = "title = " <> quote title
    toParam (ParentEq fid) = quote fid <> " in parents"
    toParam Untrashed = "trashed = false"
    toParam (p `And` q) = "(" <> toParam p <> ") and (" <> toParam q <> ")"
    toParam (p `Or` q) = "(" <> toParam p <> ") or (" <> toParam q <> ")"

    quote :: Text -> ByteString
    quote = ("'" <>) . (<> "'") . encodeUtf8

newFolder :: FileId -> Text -> Api File
newFolder parent title = do
    modified <- liftIO $ getCurrentTime

    return $ New $ FileData
        { fileTitle = title
        , fileModified = modified
        , fileParents = [parent]
        , fileTrashed = False
        , fileSize = Nothing
        , fileDownloadUrl = Nothing
        , fileMimeType = "application/vnd.google-apps.folder"
        }

newFile :: FileId -> FilePath -> Api File
newFile parent filePath = do
    modified <- liftIO $ getModificationTime filePath

    return $ New $ FileData
        { fileTitle = T.pack $ takeFileName filePath
        , fileModified = modified
        , fileParents = [parent]
        , fileTrashed = False
        , fileSize = Nothing
        , fileDownloadUrl = Nothing
        , fileMimeType = ""
        }
