{-# LANGUAGE OverloadedStrings #-}

module Server
    ( webApp
    ) where

import Servant (Server, Proxy(..), (:<|>)(..), serve)
import Network.Wai(Application)
import Network.Wai.Handler.Warp(run)
import Data.Text (Text)
import Database.PostgreSQL.Simple (Connection)

import Router (UserAPI(..))
import Handler (login, register, getUser)

server :: Connection -> Text-> Server UserAPI
server conn authUrl =  login conn authUrl :<|> register conn authUrl :<|> getUser authUrl

userAPI :: Proxy UserAPI
userAPI = Proxy

app :: Connection -> Text -> Application
app conn authUrl = serve userAPI $ server conn authUrl

webApp :: Int -> Connection -> Text ->  IO ()
webApp port conn authUrl = run port $ app conn authUrl