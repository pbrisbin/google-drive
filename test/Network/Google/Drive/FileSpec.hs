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
        runApiSpec $ \folder -> do
            let fd = setParent folder $ newFile "test-file" Nothing

            file <- createFile fd
            fileTitle (fileData file) `shouldBe` "test-file"

            mfile <- getFile $ fileId file
            fmap (fileTitle . fileData) mfile `shouldBe` Just "test-file"

    it "can update an existing file" $ do
        runApiSpec $ \folder -> do
            file <- createFile $ setParent folder $ newFile "test-file" Nothing
            let fd = fileData file

            file' <- updateFile (fileId file) $
                fd { fileTitle = "test-file-updated" }

            fileTitle (fileData file') `shouldBe` "test-file-updated"

    it "can delete a file" $ do
        runApiSpec $ \folder -> do
            file <- createFile $ setParent folder $ newFile "test-file" Nothing

            deleteFile file

            getFile (fileId file) `shouldReturn` Nothing
