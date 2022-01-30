{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

module IDP.AzureAD where
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

azureADKey :: OAuth2
azureADKey = OAuth2
  { oauthClientId            = ""
  , oauthClientSecret        = Just ""
  , oauthCallback            = Just [uri|http://localhost:9988/oauth2/callback|]
  , oauthOAuthorizeEndpoint  = [uri|https://login.windows.net/common/oauth2/authorize|]
  , oauthAccessTokenEndpoint = [uri|https://login.windows.net/common/oauth2/token|]
  }

data AzureAD = AzureAD deriving (Show, Generic)

instance Hashable AzureAD

instance IDP AzureAD

instance HasLabel AzureAD

instance HasTokenRefreshReq AzureAD where
  tokenRefreshReq _ mgr = refreshAccessToken mgr azureADKey

instance HasTokenReq AzureAD where
  tokenReq _ mgr = fetchAccessToken mgr azureADKey

instance HasUserReq AzureAD where
  userReq _ mgr at = do
    re <- authGetJSON mgr at userInfoUri
    return (second toLoginUser re)

instance HasAuthUri AzureAD where
  authUri _ = createCodeUri azureADKey [ ("state", "AzureAD.test-state-123")
                                       , ("scope", "openid,profile")
                                       , ("resource", "https://graph.microsoft.com")
                                       ]

newtype AzureADUser = AzureADUser { mail :: Text } deriving (Show, Generic)

instance FromJSON AzureADUser where
    parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = camelTo2 '_' }

userInfoUri :: URI
userInfoUri = [uri|https://graph.microsoft.com/v1.0/me|]

toLoginUser :: AzureADUser -> LoginUser
toLoginUser ouser = LoginUser { loginUserName = mail ouser }
