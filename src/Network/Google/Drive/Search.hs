{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
-- |
--
-- Searching for files on your drive.
--
-- https://developers.google.com/drive/web/search-parameters
--
module Network.Google.Drive.Search
    ( listFiles
    , listVisibleContents

    -- * Building Queries
    , Query
    , Field(..)
    , QueryValue(..)
    , (?=)
    , (?!=)
    , (?<)
    , (?<=)
    , (?>)
    , (?>=)
    , (?&&)
    , (?||)
    , qIn
    , qHas
    , qContains
    , qAnd
    , qOr
    , qNot
    ) where

import Network.Google.Api
import Network.Google.Drive.DateTime
import Network.Google.Drive.File

#if __GLASGOW_HASKELL__ < 710
import Control.Applicative ((<$>), (<$>))
#endif

import Control.Monad (mzero)
import Data.Aeson
import Data.Char (toLower)
import Data.Monoid ((<>))
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8)
import Data.Time (UTCTime)

import qualified Data.Text as T

type Query = Text

-- | Queriable fields
data Field
    = Title
    | FullText
    | MimeType
    | ModifiedDate
    | LastViewedByMeDate
    | Trashed
    | Starred
    | Parents
    | Owners
    | Writers
    | Readers
    | SharedWithMe
    | Properties
    deriving Show

-- | Type class for values which can be used in queries
class QueryValue a where
    escapeValue :: a -> Text

instance QueryValue Text where
    escapeValue t = "'" <> t <> "'"

instance QueryValue Bool where
    escapeValue True = "true"
    escapeValue False = "false"

instance QueryValue UTCTime where
    escapeValue = formatDateTime

newtype Items = Items [File]

instance FromJSON Items where
    parseJSON (Object o) = Items <$> (mapM parseJSON =<< o .: "items")
    parseJSON _ = mzero

-- | Perform a search as specified by the @Query@
listFiles :: Query -> Api [File]
listFiles query = do
    Items items <- getJSON (baseUrl <> "/files")
        [ ("q", Just $ encodeUtf8 query)
        , ("maxResults", Just "1000")
        ]

    return items

-- | List all not-trashed files within the given folder
listVisibleContents :: File -> Api [File]
listVisibleContents folder =
    listFiles $ (fileId folder) `qIn` Parents `qAnd` Trashed ?= False

-- | The content of a string or boolean is equal to the other
(?=) :: QueryValue a => Field -> a -> Query
(?=) = qOp "="

-- | The content of a string or boolean is not equal to the other
(?!=) :: QueryValue a => Field -> a -> Query
(?!=) = qOp "!="

-- | A date is earlier than another
(?<) :: QueryValue a => Field -> a -> Query
(?<) = qOp "<"

-- | A date is earlier than or equal to another
(?<=) :: QueryValue a => Field -> a -> Query
(?<=) = qOp "<="

-- | A date is later than another
(?>) :: QueryValue a => Field -> a -> Query
(?>) = qOp ">"

-- | A date is later than or equal to another
(?>=) :: QueryValue a => Field -> a -> Query
(?>=) = qOp ">="

-- | An element is contained within a collection
--
-- Used for @Parents@, @Owners@, @Writers@, and @Readers@.
--
-- Note the reversed arguments such that infix usage makes sense.
--
qIn :: QueryValue a => a -> Field -> Query
qIn v f = T.intercalate " " [escapeValue v, "in", escapeField f]

-- | A collection contains an element matching the parameters.
--
-- Used for @Properties@.
--
qHas :: QueryValue a => Field -> a -> Query
qHas = qOp "has"

-- | The content of one string is present in the other
--
-- Used for @Title@, @FullText@, and @MimeType@.
--
qContains :: QueryValue a => Field -> a -> Query
qContains = qOp "contains"

infixr 3 ?&&

-- | Return files that match both clauses
(?&&) :: Query -> Query -> Query
q ?&& p = "(" <> p <> ") and (" <> q <> ")"

infixr 3 `qAnd`

-- | Return files that match both clauses
qAnd :: Query -> Query -> Query
qAnd = (?&&)

infixr 2 ?||

-- | Return files that match either clause
(?||) :: Query -> Query -> Query
q ?|| p = "(" <> p <> ") or (" <> q <> ")"

infixr 2 `qOr`

-- | Return files that match either clause
qOr :: Query -> Query -> Query
qOr = (?||)

{-# DEPRECATED qAnd "Use ?&& instead" #-}
{-# DEPRECATED qOr "Use ?|| instead" #-}

-- | Negates a search clause
qNot :: Query -> Query
qNot q = "not " <> "(" <> q <> ")"

qOp :: QueryValue a => Text -> Field -> a -> Query
qOp x f v = T.intercalate " " [escapeField f, x, escapeValue v]

escapeField :: Field -> Text
escapeField = T.pack . lowerCase . show
  where
    lowerCase [] = []
    lowerCase (x:xs) = toLower x : xs

baseUrl :: URL
baseUrl = "https://www.googleapis.com/drive/v2"
