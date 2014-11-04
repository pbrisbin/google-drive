module Network.Google.Drive.Upload.Progress
    ( reportProgress
    , reportProgressWith
    , defaultFormatter
    , simpleFormatter
    ) where

import Control.Monad (when)
import Control.Monad.IO.Class
import Data.ByteString (ByteString)
import Data.Conduit (Conduit, await, yield)
import Data.Monoid ((<>))
import Data.Time (UTCTime, diffUTCTime, getCurrentTime)
import System.IO (hFlush, stdout)

import qualified Data.ByteString as BL

type Formatter = Progress -> String

data Progress = Progress
    { progressStart :: !UTCTime
    , progressNow :: !UTCTime
    , progressCurrent :: !Int
    , progressTotal :: !Int
    }

reportProgress :: MonadIO m => Int -> Conduit ByteString m ByteString
reportProgress = reportProgressWith defaultFormatter

reportProgressWith :: MonadIO m
                   => Formatter
                   -> Int
                   -> Conduit ByteString m ByteString
reportProgressWith formatter total = do
    now <- liftIO $ getCurrentTime

    loop Progress
        { progressStart = now
        , progressNow = now
        , progressCurrent = 0
        , progressTotal = total
        }

  where
    loop p = do
        mv <- await

        case mv of
            Nothing -> return ()
            Just v -> do
                now <- liftIO $ getCurrentTime

                let p' = increment (BL.length v) now p

                -- report progress every 100 bytes
                when (progressCurrent p `mod` 100 == 0) $ liftIO $ do
                    putStr $ formatter p' <> "\r"
                    hFlush stdout

                yield v

                loop p'

    increment bytes now p = p
        { progressNow = now
        , progressCurrent = progressCurrent p + bytes
        }

defaultFormatter :: Formatter
defaultFormatter p@Progress{..} =
    "  [" <> progress <> "]" <>
    layout 25
        [ show kbs <> " KB/s"
        , show seconds <> "s remaining"
        ]
  where
    progress = simpleFormatter p

    layout _ [] = ""
    layout n (x:xs) = ", " <> pad n (take n x) <> layout n xs

    kbs :: Integer
    kbs = round $ speed `divide` (1000 :: Integer)

    speed :: Integer
    speed = round $ progressCurrent `divide` elapsed

    seconds :: Integer
    seconds = round $ remaining `divide` speed

    elapsed :: Integer
    elapsed = round $ diffUTCTime progressNow progressStart

    remaining :: Int
    remaining = progressTotal - progressCurrent

    divide :: (Integral a, Integral b) => a -> b -> Double
    divide a b = fromIntegral a / fromIntegral b

simpleFormatter :: Formatter
simpleFormatter Progress{..} =
    pad maxLen (show progressCurrent) <> "/" <> total
  where
    maxLen = length $ total
    total = show progressTotal

pad :: Int -> String -> String
pad l v = replicate (l - length v) ' ' <> v
