{-# LANGUAGE PackageImports #-}
module Network.Google.OAuth2
    ( OAuth2Client(..)
    , OAuth2Scope
    , OAuth2Tokens(..)
    , generateTokens
    ) where

import Control.Monad (void)
import Data.Monoid ((<>))

-- TODO: inline this functionality
import "handa-gdata" Network.Google.OAuth2

import qualified Control.Exception as E

-- | Find or generate refreshed OAuth2 tokens.
generateTokens :: OAuth2Client
               -> [OAuth2Scope]
               -> Bool            -- ^ Ignore cache?
               -> FilePath        -- ^ File in which to cache the token
               -> IO OAuth2Tokens -- ^ Refreshed token
generateTokens client scopes force tokenFile = do
    tokens <- if force
        then newTokens client scopes
        else fromMaybeM (newTokens client scopes) (cachedTokens tokenFile)

    refreshed <- refreshTokens client tokens

    void $ cacheTokens tokenFile refreshed

    return refreshed

newTokens :: OAuth2Client -> [OAuth2Scope] -> IO OAuth2Tokens
newTokens client scopes = do
    let permissionUrl = formUrl client scopes

    putStrLn $ rule 80
    putStrLn "Please visit the following URL to retrieve a verification code:"
    putStrLn ""
    putStrLn $ "  " <> permissionUrl
    putStrLn ""
    putStr "Enter your verification code: "
    code <- getLine
    putStrLn $ rule 80

    exchangeCode client code

  where
    rule :: Int -> String
    rule n = map (const '-') [1..n]

cachedTokens :: FilePath -> IO (Maybe OAuth2Tokens)
cachedTokens tokenFile = do
    result <- fmap (fmap reads) $ try $ readFile tokenFile

    return $ case result of
        Right ((t,_):_) -> Just t
        _ -> Nothing

cacheTokens :: FilePath -> OAuth2Tokens -> IO OAuth2Tokens
cacheTokens tokenFile t = fmap (const t) $ try $ writeFile tokenFile (show t)

fromMaybeM :: Monad m => m b -> m (Maybe b) -> m b
fromMaybeM mb ma = ma >>= maybe mb return

try :: IO a -> IO (Either E.IOException a)
try = E.try
