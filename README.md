# Google Drive

Google Drive API access

## Stability

As we are still pre-1.0, I consider all types and interfaces freely changeable
in any way accompanied by only a minor version bump.

I apologize if this breaks your code. Update at your own risk.

## Installation

```
% cabal install google-drive
```

## Usage

```haskell
-- Top level re-exports all the sub-modules
import Network.Google.Drive

import Control.Monad (forM_)
import Data.Conduit (($$+-))

main :: IO ()
main = do
    -- See my separate google-oauth2 library
    let token = undefined

    -- runApi would return an Either ApiError a
    -- runApi_ discards the a and raises any errors, useful only in toy examples
    runApi_ token $ do
        -- "root" is an alias for the ID of the Drive itself
        root <- getFile "root"

        -- Note: This is already available as listVisibleContents
        items <- listFiles $ (fileId root) `qIn` Parents `qAnd` Trashed ?= False

        forM_ items $ \item -> do
            -- Stream the content to ./<file-title>
            let sink = ($$+- sinkFile (localPath item))

            downloadFile item sink
```

See the tests and haddocks for more information.

## Developing and Tests

```
% cp .env{.example,} # and edit it accordingly
% cabal install --dependencies-only --enable-tests -j
% cabal test
```
