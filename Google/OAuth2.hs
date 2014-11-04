module Network.Google.OAuth2
    ( OAuth2Client(..)
    , OAuth2Code
    , OAuth2Scope
    , OAuth2Tokens(..)
    , generateTokens

    -- * Lower-level actions
    , promptForCode
    , exchangeCode
    , refreshTokens
    ) where

import Control.Applicative ((<$>),(<*>))
import Control.Monad (mzero, void)
import Data.Aeson
import Data.ByteString.Lazy (ByteString)
import Data.List (intercalate)
import Data.Maybe (fromJust)
import Data.Monoid ((<>))
import Network.HTTP.Conduit
    ( Request(..)
    , RequestBody(..)
    , Response(..)
    , httpLbs
    , parseUrl
    , withManager
    )
import Network.HTTP.Base (urlEncode)
import Network.HTTP.Types (hContentType)
import System.IO (hFlush, stdout)

import qualified Control.Exception as E
import qualified Data.ByteString.Lazy.Char8 as C8

type OAuth2Code = String
type OAuth2Scope = String

data OAuth2Client = OAuth2Client
    { clientId :: String
    , clientSecret :: String
    }
    deriving (Read, Show)

data OAuth2Tokens = OAuth2Tokens
    { accessToken :: String
    , refreshToken :: String
    , expiresIn :: Int -- ^ Seconds, minutes? Unclear.
    , tokenType :: String
    }
    deriving (Read, Show)

instance FromJSON OAuth2Tokens where
    parseJSON (Object o) = OAuth2Tokens
        <$> o .: "access_token"
        <*> o .: "refresh_token"
        <*> o .: "expires_in"
        <*> o .: "token_type"

    parseJSON _ = mzero

-- Used only when refreshing a token, where the response lacks the originally
-- supplied refresh_token
data RefreshResponse = RefreshResponse
    { rAccessToken :: String
    , rExpiresIn :: Int
    , rTokenType :: String
    }

instance FromJSON RefreshResponse where
    parseJSON (Object o) = RefreshResponse
        <$> o .: "access_token"
        <*> o .: "expires_in"
        <*> o .: "token_type"

    parseJSON _ = mzero

toOAuth2Tokens :: String -> RefreshResponse -> OAuth2Tokens
toOAuth2Tokens token RefreshResponse{..} =
    OAuth2Tokens
        { accessToken = rAccessToken
        , refreshToken = token
        , expiresIn = rExpiresIn
        , tokenType = rTokenType
        }

redirectUri :: String
redirectUri = "urn:ietf:wg:oauth:2.0:oob"

formUrl :: OAuth2Client -> [OAuth2Scope] -> String
formUrl client scopes =
    "https://accounts.google.com/o/oauth2/auth"
    <> "?response_type=code"
    <> "&client_id=" <> clientId client
    <> "&redirect_uri=urn:ietf:wg:oauth:2.0:oob"
    <> "&scope=" <> intercalate "+" (map urlEncode scopes)

-- | Find or generate refreshed OAuth2 tokens
generateTokens :: OAuth2Client
               -> [OAuth2Scope]
               -> Bool            -- ^ Ignore cache?
               -> FilePath        -- ^ File in which to cache the token
               -> IO OAuth2Tokens -- ^ Refreshed token
generateTokens client scopes force tokenFile = do
    cached <- cachedTokens tokenFile
    tokens <- case (force, cached) of
        (False, Just t) -> return t
        _ -> do
            code <- promptForCode client scopes
            exchangeCode client code

    refreshed <- refreshTokens client tokens

    void $ cacheTokens tokenFile refreshed

    return refreshed

promptForCode :: OAuth2Client -> [OAuth2Scope] -> IO String
promptForCode client scopes = do
    let permissionUrl = formUrl client scopes

    putStrLn ""
    putStrLn "Visit the following URL to retrieve a verification code:"
    putStrLn ""
    putStrLn $ "  " <> permissionUrl
    putStrLn ""
    putStr   "Verification code: "
    hFlush stdout

    getLine

exchangeCode :: OAuth2Client -> OAuth2Code -> IO OAuth2Tokens
exchangeCode client code = postTokens $
       "client_id=" <> clientId client
    <> "&client_secret=" <> clientSecret client
    <> "&grant_type=authorization_code"
    <> "&redirect_uri=" <> redirectUri
    <> "&code=" <> code

refreshTokens :: OAuth2Client -> OAuth2Tokens -> IO OAuth2Tokens
refreshTokens client tokens = do
    refreshed <- postTokens $
           "client_id=" <> clientId client
        <> "&client_secret=" <> clientSecret client
        <> "&grant_type=refresh_token"
        <> "&refresh_token=" <> refreshToken tokens

    return $ toOAuth2Tokens (refreshToken tokens) refreshed

postTokens :: FromJSON a => String -> IO a
postTokens body = do
    request <- parseUrl "https://accounts.google.com/o/oauth2/token"

    fmap unsafeDecode $ withManager $ httpLbs $ request
        { method = "POST"
        , requestHeaders =
            [(hContentType, "application/x-www-form-urlencoded")]
        , requestBody = RequestBodyLBS $ C8.pack body
        }

cachedTokens :: FilePath -> IO (Maybe OAuth2Tokens)
cachedTokens tokenFile = do
    result <- fmap (fmap reads) $ try $ readFile tokenFile

    return $ case result of
        Right ((t,_):_) -> Just t
        _ -> Nothing

cacheTokens :: FilePath -> OAuth2Tokens -> IO OAuth2Tokens
cacheTokens tokenFile t = fmap (const t) $ try $ writeFile tokenFile (show t)

-- With token responses, we assume that if we don't get an HTTP exception, then
-- the response body will parse correctly.
unsafeDecode :: FromJSON a => Response ByteString -> a
unsafeDecode = fromJust . decode . responseBody

try :: IO a -> IO (Either E.IOException a)
try = E.try
