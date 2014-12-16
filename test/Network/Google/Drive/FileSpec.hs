{-# LANGUAGE OverloadedStrings #-}
module Network.Google.Drive.FileSpec
    ( main
    , spec
    ) where

import SpecHelper

main :: IO ()
main = hspec spec

spec :: Spec
spec = describe "Network.Google.Drive.File" $ do
    it "can create a file and get it back" $ do
        now <- getCurrentTime

        runApiSpec $ \folder -> do
            let fd = setParent folder $ newFile "test-file" now

            file <- createFile fd
            fileTitle (fileData file) `shouldBe` "test-file"

            mfile <- getFile $ fileId file
            fmap (fileTitle . fileData) mfile `shouldBe` Just "test-file"

    it "can update an existing file" $ do
        now <- getCurrentTime

        runApiSpec $ \folder -> do
            file <- createFile $ setParent folder $ newFile "test-file" now
            let fd = fileData file
                fd' = fd { fileTitle = "test-file-updated" }

            file' <- updateFile $ file { fileData = fd' }

            fileTitle (fileData file') `shouldBe` "test-file-updated"

    it "can delete a file" $ do
        now <- getCurrentTime

        runApiSpec $ \folder -> do
            file <- createFile $ setParent folder $ newFile "test-file" now

            deleteFile file

            (`shouldBe` Nothing) =<< getFile (fileId file)
