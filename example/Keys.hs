{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

-- |
module Keys where

import           Data.ByteString                ( ByteString )
import           Network.OAuth.OAuth2
import           URI.ByteString.QQ

weiboKey :: OAuth2
weiboKey = OAuth2
  { oauthClientId            = "xxxxxxxxxxxxxxx"
  , oauthClientSecret        = Just "xxxxxxxxxxxxxxxxxxxxxx"
  , oauthCallback            = Just [uri|http://127.0.0.1:9988/oauthCallback|]
  , oauthOAuthorizeEndpoint  = [uri|https://api.weibo.com/oauth2/authorize|]
  , oauthAccessTokenEndpoint = [uri|https://api.weibo.com/oauth2/access_token|]
  }

-- | http://developer.github.com/v3/oauth/
githubKey :: OAuth2
githubKey = OAuth2
  { oauthClientId            = "xxxxxxxxxxxxxxx"
  , oauthClientSecret        = Just "xxxxxxxxxxxxxxxxxxxxxx"
  , oauthCallback            = Just [uri|http://127.0.0.1:9988/githubCallback|]
  , oauthOAuthorizeEndpoint  = [uri|https://github.com/login/oauth/authorize|]
  , oauthAccessTokenEndpoint =
    [uri|https://github.com/login/oauth/access_token|]
  }

-- | oauthCallback = Just "https://developers.google.com/oauthplayground"
googleKey :: OAuth2
googleKey = OAuth2
  { oauthClientId            = "xxxxxxxxxxxxxxx.apps.googleusercontent.com"
  , oauthClientSecret        = Just "xxxxxxxxxxxxxxxxxxxxxx"
  , oauthCallback            = Just [uri|http://127.0.0.1:9988/googleCallback|]
  , oauthOAuthorizeEndpoint  = [uri|https://accounts.google.com/o/oauth2/auth|]
  , oauthAccessTokenEndpoint = [uri|https://www.googleapis.com/oauth2/v3/token|]
  }

facebookKey :: OAuth2
facebookKey = OAuth2
  { oauthClientId            = "xxxxxxxxxxxxxxx"
  , oauthClientSecret        = Just "xxxxxxxxxxxxxxxxxxxxxx"
  , oauthCallback            = Just [uri|http://t.haskellcn.org/cb|]
  , oauthOAuthorizeEndpoint  = [uri|https://www.facebook.com/dialog/oauth|]
  , oauthAccessTokenEndpoint =
    [uri|https://graph.facebook.com/v2.3/oauth/access_token|]
  }

doubanKey :: OAuth2
doubanKey = OAuth2
  { oauthClientId            = "xxxxxxxxxxxxxxx"
  , oauthClientSecret        = Just "xxxxxxxxxxxxxxxxxxxxxx"
  , oauthCallback            = Just [uri|http://localhost:9999/oauthCallback|]
  , oauthOAuthorizeEndpoint  = [uri|https://www.douban.com/service/auth2/auth|]
  , oauthAccessTokenEndpoint = [uri|https://www.douban.com/service/auth2/token|]
  }

fitbitKey :: OAuth2
fitbitKey = OAuth2
  { oauthClientId            = "xxxxxx"
  , oauthClientSecret        = Just "xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx"
  , oauthCallback            = Just [uri|http://localhost:9988/oauth2/callback|]
  , oauthOAuthorizeEndpoint  = [uri|https://www.fitbit.com/oauth2/authorize|]
  , oauthAccessTokenEndpoint = [uri|https://api.fitbit.com/oauth2/token|]
  }

-- fix key from your application edit page
-- https://stackapps.com/apps/oauth
stackexchangeAppKey :: ByteString
stackexchangeAppKey = "xxxxxx"

stackexchangeKey :: OAuth2
stackexchangeKey = OAuth2
  { oauthClientId            = "xx"
  , oauthClientSecret        = Just "xxxxxxxxxxxxxxx"
  , oauthCallback            = Just [uri|http://c.haskellcn.org/cb|]
  , oauthOAuthorizeEndpoint  = [uri|https://stackexchange.com/oauth|]
  , oauthAccessTokenEndpoint =
    [uri|https://stackexchange.com/oauth/access_token|]
  }
dropboxKey :: OAuth2
dropboxKey = OAuth2
  { oauthClientId            = "xxx"
  , oauthClientSecret        = Just "xxx"
  , oauthCallback            = Just [uri|http://localhost:9988/oauth2/callback|]
  , oauthOAuthorizeEndpoint  = [uri|https://www.dropbox.com/1/oauth2/authorize|]
  , oauthAccessTokenEndpoint = [uri|https://api.dropboxapi.com/oauth2/token|]
  }

oktaKey :: OAuth2
oktaKey = OAuth2
  { oauthClientId            = "xxx"
  , oauthClientSecret        = Just "xxx"
  , oauthCallback            = Just [uri|http://localhost:9988/oauth2/callback|]
  , oauthOAuthorizeEndpoint  =
    [uri|https://dev-148986.oktapreview.com/oauth2/v1/authorize|]
  , oauthAccessTokenEndpoint =
    [uri|https://dev-148986.oktapreview.com/oauth2/v1/token|]
  }

azureADKey :: OAuth2
azureADKey = OAuth2
  { oauthClientId            = "xxx"
  , oauthClientSecret        = Just "xxx"
  , oauthCallback            = Just [uri|http://localhost:9988/oauth2/callback|]
  , oauthOAuthorizeEndpoint  =
    [uri|https://login.windows.net/common/oauth2/authorize|]
  , oauthAccessTokenEndpoint =
    [uri|https://login.windows.net/common/oauth2/token|]
  }

zohoKey :: OAuth2
zohoKey = OAuth2
  { oauthClientId            = "xxx"
  , oauthClientSecret        = Just "xxx"
  , oauthCallback            = Just [uri|http://localhost:9988/oauth2/callback|]
  , oauthOAuthorizeEndpoint  = [uri|https://accounts.zoho.com/oauth/v2/auth|]
  , oauthAccessTokenEndpoint = [uri|https://accounts.zoho.com/oauth/v2/token|]
  }

auth0Key :: OAuth2
auth0Key = OAuth2
  { oauthClientId            = "xxx"
  , oauthClientSecret        = Just "xxx"
  , oauthCallback            = Just [uri|http://localhost:9988/oauth2/callback|]
  , oauthOAuthorizeEndpoint  = [uri|https://foo.auth0.com/authorize|]
  , oauthAccessTokenEndpoint = [uri|https://foo.auth0.com/oauth/token|]
  }

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
