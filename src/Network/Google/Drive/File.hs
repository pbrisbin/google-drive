{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
-- |
--
-- Methods for working with File resources on Google Drive.
--
-- https://developers.google.com/drive/v2/reference/files
--
-- See @"Network.Google.Drive.Upload"@ for uploading files.
--
module Network.Google.Drive.File
    (
    -- * File Resource
      File(..)
    , FileId
    , FileData(..)
    , fileId
    , fileData
    , isFolder
    , localPath
    , uploadMethod
    , uploadPath
    , uploadData

    -- * Search
    , Query(..)
    , Items(..)
    , listFiles

    -- * Actions
    , getFile
    , deleteFile

    -- * Utilities
    , newFile
    , createFolder
    ) where

import Control.Applicative ((<$>), (<*>))
import Control.Monad (mzero, void)
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

-- | Metadata about Files on your Drive
data FileData = FileData
    { fileTitle :: !Text
    , fileModified :: !UTCTime
    , fileParents :: ![FileId]
    , fileTrashed :: !Bool
    , fileSize :: !(Maybe Int)
    , fileDownloadUrl :: !(Maybe Text)
    , fileMimeType :: !Text
    }

data File
    = File FileId FileData -- ^ A File that exists
    | New FileData         -- ^ A File you intend to create

instance Eq File where
    File a _ == File b _ = a == b
    _ == _ = False

instance Show File where
    show (File fi fd) = T.unpack $ fileTitle fd <> " (" <> fi <> ")"
    show (New fd) = show $ File "new" fd

-- | N.B. it is an error to ask for the @fileId@ of a new file
fileId :: File -> FileId
fileId (File x _) = x
fileId _ = error "Cannot get fileId for new File"

fileData :: File -> FileData
fileData (File _ x) = x
fileData (New x) = x

-- | HTTP Method to use for uploading content for this file
uploadMethod :: File -> Method
uploadMethod (File _ _) = "PUT"
uploadMethod (New _) = "POST"

-- | Path to use for uploading content for this file
uploadPath :: File -> Path
uploadPath (File fid _) = "/files/" <> T.unpack fid
uploadPath (New _) = "/files"

-- | HTTP Body to send when uploading content for this file
--
-- Currently a synonym for @fileData@.
--
uploadData :: File -> FileData
uploadData = fileData

-- | What to name this file if downloaded
--
-- Currently just the @fileTitle@
--
localPath :: File -> FilePath
localPath = T.unpack . fileTitle . fileData

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
        , "parents" .= map (\p -> object ["id" .= p]) fileParents
        , "labels" .= object ["trashed" .= fileTrashed]
        , "mimeType" .= fileMimeType
        ]

instance FromJSON Items where
    parseJSON (Object o) = Items <$> (mapM parseJSON =<< o .: "items")
    parseJSON _ = mzero

isFolder :: File -> Bool
isFolder = (== folderMimeType) . fileMimeType . fileData

-- | Search query parameter
--
-- Currently only a small subset of queries are supported
--
data Query
    = TitleEq Text
    | ParentEq FileId
    | Untrashed
    | Query `And` Query
    | Query `Or` Query

baseUrl :: URL
baseUrl = "https://www.googleapis.com/drive/v2"

-- | Get @File@ data by @FileId@
--
-- @\"root\"@ can be used to get information on the Drive itself
--
getFile :: FileId -> Api File
getFile fid = getJSON (baseUrl <> "/files/" <> T.unpack fid) []

-- | Delete a @File@
deleteFile :: File -> Api ()
deleteFile (New _) = return ()
deleteFile (File fid _) = void $ requestLbs
    (baseUrl <> "/files/" <> T.unpack fid) $ setMethod "DELETE"

-- | Perform a search as specified by the @Query@
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

-- | Build a new @File@
--
-- N.B. This does not create the file.
--
-- The file is defined as within the given parent, and has some information
-- (currently title and modified) taken from the local file
--
newFile :: FileId   -- ^ Parent
        -> FilePath
        -> Api File
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

-- | Create a new remote folder
createFolder :: FileId -- ^ Parent
             -> Text   -- ^ Title to give the folder
             -> Api File
createFolder parent title = do
    modified <- liftIO $ getCurrentTime

    postJSON (baseUrl <> "/files") [] $ FileData
        { fileTitle = title
        , fileModified = modified
        , fileParents = [parent]
        , fileTrashed = False
        , fileSize = Nothing
        , fileDownloadUrl = Nothing
        , fileMimeType = folderMimeType
        }

folderMimeType :: Text
folderMimeType = "application/vnd.google-apps.folder"
