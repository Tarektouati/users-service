{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Handler (login, register, getUser) where

import Database.PostgreSQL.Simple as PG (Connection)
import Servant (Handler, err500, err401, errBody, throwError)
import Control.Monad.IO.Class (liftIO)
import Data.List as List (length)
import Data.Text (Text, pack, splitOn, unpack, append)
import Data.Text.Encoding as TextE (encodeUtf8, decodeUtf8)
import Network.HTTP.Simple as HTTP (httpJSON, getResponseBody)
import Data.UUID.V4 (nextRandom)

import DB as DB (UserDB(..), findUserByEmail, saveUser, toPublicUser)
import Types (Response(..), Login(..), Register(..))
import Token  (getTokenRequest, getUserRequest)
import Utils (zero64)
import Crypt (hashPassword, comparePassword)



-- Login Handler
login :: PG.Connection -> Text->  Login -> Handler Types.Response
login c authUrl Login{email=e, password=p} = do
  users <- liftIO $ DB.findUserByEmail c e
  if List.length users == 0
    then throwError $ err500 { errBody = "User not found" }
    else do
        let (user: _) = users
        let UserDB{password = dbPassword} = user
          in if comparePassword p dbPassword 
              then do
                let publicUser = toPublicUser user
                let request = getTokenRequest publicUser authUrl
                response <- HTTP.httpJSON request
                let authResp = HTTP.getResponseBody response :: Types.Response
                return $ Types.Response "User LoggedIn" True (token authResp) $ Just $ publicUser
              else throwError $ err500 { errBody = "User not found" }
           
-- Register Handler                         
register :: PG.Connection -> Text->  Register -> Handler Types.Response
register c authUrl Register{email=e,firstName=f,lastName=l,password=p} = do
  users <- liftIO $ DB.findUserByEmail c e
  if List.length users > 0
    then  throwError $ err500 { errBody = "Email already used" }
    else do
      passwordHash <- liftIO $ hashPassword p
      case passwordHash of
        Nothing -> throwError $ err500 { errBody = "Error occured while hashing password" }
        Just x -> do
          uuidv4 <- liftIO $ nextRandom
          let userDB = DB.UserDB{email=e,firstName=f,lastName=l,password= TextE.decodeUtf8 x,id=uuidv4}
              publicUser = toPublicUser userDB
          rows <-  liftIO $ DB.saveUser c userDB
          if rows > zero64
            then do
             let request = getTokenRequest publicUser authUrl
             response <- HTTP.httpJSON request 
             let authResp = HTTP.getResponseBody response :: Types.Response
              in return $ Types.Response "User registred" True (token authResp) $ Just $ publicUser
            else throwError $ err500 { errBody = "Error occured while saving user" }


-- GetUser Handler                         
getUser ::  Text -> Maybe Text -> Handler Types.Response
getUser _ Nothing  = 
             throwError $ err401 { errBody = "Token not found in headers" }
getUser authUrl (Just authToken)  = do
  let (_:token:_) = splitOn "Bearer " authToken
      request = getUserRequest token authUrl
  response <- HTTP.httpJSON request 
  let Types.Response{user = u, success=s} = HTTP.getResponseBody response :: Types.Response
    in  if s
      then return $ Types.Response "user found" True (Just token) u
      else throwError $ err401 { errBody = "User not found or incorrect token" }