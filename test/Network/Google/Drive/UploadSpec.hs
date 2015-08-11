{-# LANGUAGE OverloadedStrings #-}
module Network.Google.Drive.UploadSpec
    ( main
    , spec
    ) where

import SpecHelper
import Data.Conduit (($$+-))
import Data.Conduit.Binary (sinkLbs)

import qualified Data.ByteString.Lazy.Char8 as C8

main :: IO ()
main = hspec spec

spec :: Spec
spec = describe "Network.Google.Drive.Upload" $ do
    it "can upload new content" $ do
        runApiSpec $ \folder -> do
            let fd = setParent folder $ newFile "test-file" Nothing

            file <- createFileWithContent fd (fSize fixture) $
                uploadSourceFile $ fPath fixture

            fileTitle (fileData file) `shouldBe` "test-file"

            downloadFile file ($$+- sinkLbs)
                `shouldReturn` Just (C8.pack $ fContent fixture)

    it "can update existing content" $ do
        runApiSpec $ \folder -> do
            let fd = setParent folder $ newFile "test-file" Nothing

            file <- createFile fd
            file' <- updateFileWithContent
                (fileId file)
                (fd { fileTitle = "test-file-updated" })
                (fSize fixture)
                (uploadSourceFile $ fPath fixture)

            fileTitle (fileData file') `shouldBe` "test-file-updated"

            downloadFile file ($$+- sinkLbs)
                `shouldReturn` Just (C8.pack $ fContent fixture)
