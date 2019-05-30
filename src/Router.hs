{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Router (UserAPI(..)) where
import Types (Login, Response, Register)
import Servant (ReqBody, JSON, Post, Get, (:>), (:<|>), Header)
import Data.Text (Text)

type UserAPI = "login" :> ReqBody '[JSON] Login :> Post '[JSON] Response
          :<|> "register" :> ReqBody '[JSON] Register :> Post '[JSON] Response
          :<|> "user" :> TokenHeader :> Get '[JSON] Response

type TokenHeader =
  Header "Authorization" Text