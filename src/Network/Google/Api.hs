module Network.Google.Api -- TODO: Client
    ( DriveApi
    , withDrive
    , withDrive_
    , module JSON.Client
    ) where

import Data.Monoid ((<>))
import JSON.Client
import Network.HTTP.Types (Header, hAuthorization)

import qualified Data.ByteString.Char8 as C8

-- | @'Api'@ with an OAuth2Token as the client configuration
type DriveApi = Api String

-- | Specialized version of @'runApi'@
withDrive :: String -> DriveApi a -> IO (Either ApiError a)
withDrive = runApi . driveClient

-- | Specialized version of @'runApi_'@
withDrive_ :: String -> DriveApi a -> IO ()
withDrive_ = runApi_ . driveClient

driveClient :: String -> Client String
driveClient token = newClient token
    `withBaseUrl` "https://www.google.apis.com"
    `withPrepare` (addHeader . authHeader)

authHeader :: String -> Header
authHeader t = (hAuthorization, C8.pack $ "Bearer " <> t)
