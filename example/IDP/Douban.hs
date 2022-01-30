{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

module IDP.Douban where
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

doubanKey :: OAuth2
doubanKey = OAuth2
  { oauthClientId            = ""
  , oauthClientSecret        = Just ""
  , oauthCallback            = Just [uri|http://localhost:9999/oauthCallback|]
  , oauthOAuthorizeEndpoint  = [uri|https://www.douban.com/service/auth2/auth|]
  , oauthAccessTokenEndpoint = [uri|https://www.douban.com/service/auth2/token|]
  }

data Douban = Douban deriving (Show, Generic)

instance Hashable Douban

instance IDP Douban

instance HasLabel Douban

instance HasTokenRefreshReq Douban where
  tokenRefreshReq _ mgr = refreshAccessToken mgr doubanKey

instance HasTokenReq Douban where
  tokenReq _ mgr = fetchAccessToken2 mgr doubanKey

instance HasUserReq Douban where
  userReq _ mgr at = do
    re <- authGetJSON mgr at userInfoUri
    return (second toLoginUser re)

instance HasAuthUri Douban where
  authUri _ = createCodeUri doubanKey [ ("state", "Douban.test-state-123")
                                        ]

data DoubanUser = DoubanUser { name :: Text
                             , uid  :: Text
                             } deriving (Show, Generic)

instance FromJSON DoubanUser where
    parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = camelTo2 '_' }

userInfoUri :: URI
userInfoUri = [uri|https://api.douban.com/v2/user/~me|]

toLoginUser :: DoubanUser -> LoginUser
toLoginUser ouser = LoginUser { loginUserName = name ouser }

