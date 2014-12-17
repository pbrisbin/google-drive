{-# LANGUAGE OverloadedStrings #-}
module SpecHelper
    ( runApiSpec

    -- * Fixtures
    , Fixture(..)
    , fixture

    -- * Re-exports
    , module Test.Hspec
    , module Test.Hspec.Expectations.Lifted
    , module Network.Google.Drive
    , getCurrentTime
    ) where

import Test.Hspec hiding
    ( expectationFailure
    , shouldBe
    , shouldContain
    , shouldMatchList
    , shouldReturn
    , shouldSatisfy
    )
import Test.Hspec.Expectations.Lifted
import Network.Google.Drive

import Control.Applicative ((<$>), (<*>))
import Data.Time (getCurrentTime)
import LoadEnv (loadEnv)
import Network.Google.OAuth2
import System.Environment (getEnv)

data Fixture = Fixture
    { fPath :: FilePath
    , fSize :: Int
    , fContent :: String
    }

fixture :: Fixture
fixture = Fixture
    { fPath = "test/files/upload.txt"
    , fSize = 14
    , fContent = "Local content\n"
    }

-- | Run an API spec and cleanup after
--
-- Create a folder and hand it to the given action. That action should place any
-- working files within the folder. The folder will be removed at the end of the
-- spec run.
--
runApiSpec :: (File -> Api a) -> IO ()
runApiSpec spec = do
    now <- getCurrentTime
    token <- getToken

    runApi_ token $ do
        Just root <- getFile "root"
        folder <- createFile $
            setParent root $ newFolder "google-drive-test" now

        spec folder `finally` deleteFile folder

  where
    f `finally` g = f >> g `catchError` \e -> g >> throwError e

getToken :: IO OAuth2Token
getToken = do
    loadEnv

    client <- OAuth2Client
        <$> getEnv "CLIENT_ID"
        <*> getEnv "CLIENT_SECRET"

    getAccessToken client driveScopes . Just =<< getEnv "CACHE_FILE"
