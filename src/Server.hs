{-# LANGUAGE OverloadedStrings #-}

module Server
    ( webApp
    ) where

import           Data.Text (Text)
import           Database.PostgreSQL.Simple (Connection)
import           Network.Wai (Application)
import           Network.Wai.Handler.Warp (run)
import           Servant ((:<|>) (..), Proxy (..), Server, serve)

import           Handler (getUser, login, register)
import           Router (UserAPI (..))

server :: Connection -> Text-> Server UserAPI
server conn authUrl =  login conn authUrl :<|> register conn authUrl :<|> getUser authUrl

userAPI :: Proxy UserAPI
userAPI = Proxy

app :: Connection -> Text -> Application
app conn authUrl = serve userAPI $ server conn authUrl

webApp :: Int -> Connection -> Text ->  IO ()
webApp port conn authUrl = run port $ app conn authUrl
