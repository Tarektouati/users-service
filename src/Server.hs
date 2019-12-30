{-# LANGUAGE OverloadedStrings #-}

module Server
    ( webApp
    ) where

import           Data.Text (Text)
import           Database.PostgreSQL.Simple (Connection)
import           Network.Wai (Application)
import           Servant ((:<|>) (..), Proxy (..), Server, serve)

import           Network.Wai.Handler.Warp (defaultSettings, runSettings, setLogger, setPort)
import           Network.Wai.Logger (withStdoutLogger)

import           Handler (getUser, login, register)
import           Router (UserAPI (..))

server :: Connection -> Text-> Server UserAPI
server conn authUrl =  login conn authUrl :<|> register conn authUrl :<|> getUser authUrl

userAPI :: Proxy UserAPI
userAPI = Proxy

app :: Connection -> Text -> Application
app conn authUrl = serve userAPI $ server conn authUrl

webApp :: Int -> Connection -> Text ->  IO ()
webApp port conn authUrl = withStdoutLogger $ \applogger -> do
    let settings = setPort port $ setLogger applogger defaultSettings
    runSettings settings $ app conn authUrl
