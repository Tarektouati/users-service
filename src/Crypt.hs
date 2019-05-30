{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Crypt 
          ( hashPassword
          , comparePassword
          ) where

import Data.Text (Text)
import Data.ByteString (ByteString)
import Data.Text.Encoding as TextE (encodeUtf8)
import Crypto.BCrypt as B (validatePassword, hashPasswordUsingPolicy, slowerBcryptHashingPolicy)

hashPassword :: Text -> IO (Maybe ByteString)
hashPassword password = B.hashPasswordUsingPolicy B.slowerBcryptHashingPolicy $ TextE.encodeUtf8 $ password

-- Compare Passwords
comparePassword :: Text -> Text -> Bool
comparePassword userPassword password = B.validatePassword (TextE.encodeUtf8 password) (TextE.encodeUtf8 $ userPassword)