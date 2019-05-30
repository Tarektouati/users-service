{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DuplicateRecordFields #-}

module DB 
        ( UserDB(..)
        , connection
        , findUserByEmail
        , saveUser
        , toPublicUser
        )where

import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromRow
import Data.Int (Int64)
import Data.Text (Text)
import Types (User(..), PublicUser(..))
import Data.Text.Encoding as TextE (encodeUtf8)
import Data.UUID (UUID(..), toText)


data UserDB = UserDB {
  id :: UUID,
  email :: Text,
  firstName :: Text,
  lastName:: Text,
  password :: Text
}

toPublicUser :: UserDB -> PublicUser
toPublicUser UserDB{email=e,firstName=f,lastName=l,id=i} = 
  PublicUser{email=e,firstName=f,lastName=l,id=i}

instance FromRow UserDB where
  fromRow = UserDB <$> field <*> field <*> field <*> field <*> field

connection :: Text ->  IO Connection
connection connString = connectPostgreSQL $ TextE.encodeUtf8 connString

findUserByEmail :: Connection -> Text -> IO [UserDB]
findUserByEmail c email = 
  query c "SELECT u.id, u.email, u.firstname, u.lastname, u.password FROM USERS as u  WHERE u.email = ?" [email]
  
saveUser :: Connection -> UserDB -> IO Int64
saveUser c UserDB{email=e,firstName=f,lastName=l,password=p,id=i} = 
  execute c "INSERT INTO USERS (email, firstname, lastname, password, id) VALUES (?,?,?,?,?)" [e,f,l,p, toText i]