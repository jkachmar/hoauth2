{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

module IDP.Github where
import           Data.Aeson
import           Data.Bifunctor
import           Data.Hashable
import           Data.Text.Lazy       (Text)
import           GHC.Generics
import           Network.OAuth.OAuth2
import           Types
import           URI.ByteString
import           URI.ByteString.QQ
import           Utils

-- | http://developer.github.com/v3/oauth/
githubKey :: OAuth2
githubKey = OAuth2
  { oauthClientId            = ""
  , oauthClientSecret        = Just ""
  , oauthCallback            = Just [uri|http://127.0.0.1:9988/githubCallback|]
  , oauthOAuthorizeEndpoint  = [uri|https://github.com/login/oauth/authorize|]
  , oauthAccessTokenEndpoint =
    [uri|https://github.com/login/oauth/access_token|]
  }


data Github = Github deriving (Show, Generic)

instance Hashable Github

instance IDP Github

instance HasLabel Github

instance HasTokenReq Github where
  tokenReq _ mgr = fetchAccessToken mgr githubKey

instance HasTokenRefreshReq Github where
  tokenRefreshReq _ mgr = refreshAccessToken mgr githubKey

instance HasUserReq Github where
  userReq _ mgr at = do
    re <- authGetJSON mgr at userInfoUri
    return (second toLoginUser re)

instance HasAuthUri Github where
  authUri _ = createCodeUri githubKey [("state", "Github.test-state-123")]

data GithubUser = GithubUser { name :: Text
                             , id   :: Integer
                             } deriving (Show, Generic)

instance FromJSON GithubUser where
    parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = camelTo2 '_' }

userInfoUri :: URI
userInfoUri = [uri|https://api.github.com/user|]

toLoginUser :: GithubUser -> LoginUser
toLoginUser guser = LoginUser { loginUserName = name guser }
