{-# LANGUAGE OverloadedStrings #-}

module Main where

import Server (webApp)
import Utils (toInt)
import System.Environment (lookupEnv)
import Data.Maybe  (fromMaybe)
import Data.Text (Text, pack)
import DB (connection)
import Control.Exception (try, SomeException)
import Database.PostgreSQL.Simple (Connection)



handler :: SomeException -> IO ()
handler ex = putStrLn $ "Caught exception: " ++ show ex

main :: IO ()
main = do
  maybePort <- lookupEnv "PORT"
  maybeAuth <- lookupEnv "AUTH_SERVICE"
  maybeDbString <- lookupEnv "PG_URl"
  let port = fromMaybe "4001" maybePort
      auth = fromMaybe "http://localhost:4002" maybeAuth
      dbString = fromMaybe "postgres://dbUser:dbPassword@localhost:5432/haskell-users" maybeDbString
  connResult <- try (connection $ pack dbString) :: IO (Either SomeException Connection)
  case connResult of
    Left ex -> putStrLn $ "Exception caught : " ++ show ex
    Right conn -> webApp (toInt port) conn $ pack auth
 