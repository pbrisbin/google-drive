-- |
--
-- Searching for files on Google Drive.
--
-- https://developers.google.com/drive/web/search-parameters
--
module Network.Google.Drive.Search
    ( Query(..)
    , getFiles
    ) where

import Data.ByteString (ByteString)
import Data.Monoid ((<>))
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8)

import Network.Google.Drive.Api
import Network.Google.Drive.File

data Query
    = TitleEq Text
    | ParentEq FileId
    | Query `And` Query
    | Query `Or` Query

getFiles :: Query -> Api [File]
getFiles query = do
    let query' =
            [ ("q", Just $ toParam query)
            , ("maxResults", Just "1000")
            ]

    mlist <- getApi "/files" query'

    return $ case mlist of
        Just (Items items) -> unTrashed items
        Nothing -> []

  where
    unTrashed :: [File] -> [File]
    unTrashed = filter (not . fileTrashed)

toParam :: Query -> ByteString
toParam (TitleEq title) = "title = " <> quote title
toParam (ParentEq fileId) = quote fileId <> " in parents"
toParam (p `And` q) = "(" <> toParam p <> ") and (" <> toParam q <> ")"
toParam (p `Or` q) = "(" <> toParam p <> ") or (" <> toParam q <> ")"

quote :: Text -> ByteString
quote = ("'" <>) . (<> "'") . encodeUtf8
