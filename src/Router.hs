{-# LANGUAGE DataKinds, DuplicateRecordFields, OverloadedStrings, TypeOperators #-}

module Router (UserAPI(..)) where
import           Data.Text (Text)
import           Servant ((:<|>), (:>), Get, Header, JSON, Post, ReqBody)
import           Types (Login, Register, Response)

type UserAPI = "login" :> ReqBody '[JSON] Login :> Post '[JSON] Response
          :<|> "register" :> ReqBody '[JSON] Register :> Post '[JSON] Response
          :<|> "user" :> TokenHeader :> Get '[JSON] Response

type TokenHeader =
  Header "Authorization" Text
