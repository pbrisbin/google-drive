{-# LANGUAGE CPP #-}
module Network.Google.Drive.DateTime
    ( formatDateTime
    ) where

#if __GLASGOW_HASKELL__ < 710
import Control.Applicative ((<$>))
#endif

import Data.Text (Text)
import Data.Time (UTCTime)
import Data.Time.Format (defaultTimeLocale, formatTime)

import qualified Data.Text as T

-- | Format @'UTCTime'@ values for the Drive API
--
-- The docs claim it must be RFC 3339 formatted, but in practice the fractional
-- seconds, @%Q@, (which the RFC states is optional) are required.
--
formatDateTime :: UTCTime -> Text
formatDateTime = T.pack . formatTime defaultTimeLocale "%FT%T%QZ"
