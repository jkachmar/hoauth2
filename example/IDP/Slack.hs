{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module IDP.Slack where

import Data.Aeson
import Data.Bifunctor
import Data.Hashable
import Data.Text.Lazy (Text)
import GHC.Generics
import Network.OAuth.OAuth2
import Types
import URI.ByteString
import URI.ByteString.QQ
import Utils

-- https://api.slack.com/authentication/sign-in-with-slack
-- https://slack.com/.well-known/openid-configuration
slackKey :: OAuth2
slackKey =
  OAuth2
    { oauthClientId = ""
    , oauthClientSecret = Just ""
    , oauthCallback = Just [uri|http://localhost:9988/oauth2/callback|]
    , oauthOAuthorizeEndpoint = [uri|https://slack.com/openid/connect/authorize|]
    , oauthAccessTokenEndpoint = [uri|https://slack.com/api/openid.connect.token|]
    }

data Slack = Slack
  deriving (Show, Generic)

instance Hashable Slack

instance IDP Slack

instance HasLabel Slack

instance HasTokenReq Slack where
  tokenReq _ mgr = fetchAccessToken mgr slackKey

instance HasTokenRefreshReq Slack where
  tokenRefreshReq _ mgr = refreshAccessToken mgr slackKey

instance HasUserReq Slack where
  userReq _ mgr at = do
    re <- authGetJSON mgr at userInfoUri
    return (second toLoginUser re)

instance HasAuthUri Slack where
  authUri _ =
    createCodeUri
      slackKey
      [ ("state", "Slack.test-state-123"),
        ( "scope",
          "openid profile email"
        )
      ]

data SlackUser = SlackUser
  { name :: Text,
    email :: Text
  }
  deriving (Show, Generic)

instance FromJSON SlackUser where
  parseJSON =
    genericParseJSON defaultOptions {fieldLabelModifier = camelTo2 '_'}

userInfoUri :: URI
userInfoUri = [uri|https://slack.com/api/openid.connect.userInfo|]

toLoginUser :: SlackUser -> LoginUser
toLoginUser ouser = LoginUser {loginUserName = name ouser}
