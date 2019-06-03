{-# LANGUAGE DataKinds, DuplicateRecordFields, OverloadedStrings, TypeOperators #-}


module Token
          ( buildAuthUrl
          , getTokenRequest
          , getUserRequest
          ) where

import           Data.Text (Text, append, unpack)
import           Data.Text.Encoding as TextE (encodeUtf8)
import           Network.HTTP.Simple as HTTP (Request (..), parseRequest_, setRequestBodyJSON, setRequestMethod)
import           Types (PublicUser (..), TokenBody (..), UserBody (..))


buildAuthUrl :: Text -> Text -> Text -> HTTP.Request
buildAuthUrl endpoint method path =
  let initReq = HTTP.parseRequest_ $ unpack $ append endpoint path
    in HTTP.setRequestMethod (TextE.encodeUtf8 method) initReq

-- Make request to auth-service POST/generate to get token
getTokenRequest :: PublicUser -> Text -> HTTP.Request
getTokenRequest user endpoint =
  (HTTP.setRequestBodyJSON $ toTokenBody user) authUrl
  where authUrl = buildAuthUrl endpoint "POST" "/generate"

getUserRequest :: Text -> Text -> HTTP.Request
getUserRequest token endpoint =
  (HTTP.setRequestBodyJSON $ toUserBody token) authUrl
  where authUrl = buildAuthUrl endpoint "POST" "/verify"


toTokenBody :: PublicUser -> TokenBody
toTokenBody pUser = TokenBody{user=pUser}

toUserBody :: Text -> UserBody
toUserBody t = UserBody{jwt=t}
