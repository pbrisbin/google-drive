-- |
--
-- A convenient re-exporting of all modules
--
module Network.Google.Drive
    ( driveScopes

    -- * Re-exports
    , module X
    ) where

import Network.Google.Api as X
import Network.Google.Drive.File as X
import Network.Google.Drive.Search as X
import Network.Google.Drive.Upload as X

-- | OAuth2 scopes to use for access to the Drive API
driveScopes :: [String]
driveScopes = ["https://www.googleapis.com/auth/drive"]
