# Google Drive

Google Drive API token

## Installation

```
% cabal install google-drive
```

## Usage

```haskell
import Control.Monad (void)
import Data.Conduit (($$+-))

import Network.Google.Drive

main :: IO ()
main = void $ runApi token $ do
    root <- getFile "root"
    items <- listFiles $ ParentEq (fileId root) `And` Untrashed

    mapM_ download items

  where
    download :: File -> Api ()
    download file = do
        let fd = fileData file

        case fileDownloadUrl $ fd of
            Nothing -> return ()
            Just url -> getSource (T.unpack url) [] $ \source ->
                source $$+- sinkFile (fileTitle fd)
```

## Developing and Tests

```
% cp .env{.example,} # and edit it accordingly
% cabal install --dependencies-only --enable-tests -j
% cabal test
```
