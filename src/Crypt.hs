{-# LANGUAGE DataKinds, DuplicateRecordFields, OverloadedStrings, TypeOperators #-}

module Crypt
          ( hashPassword
          , comparePassword
          ) where

import           Crypto.BCrypt as B (hashPasswordUsingPolicy, slowerBcryptHashingPolicy, validatePassword)
import           Data.ByteString (ByteString)
import           Data.Text (Text)
import           Data.Text.Encoding as TextE (encodeUtf8)

hashPassword :: Text -> IO (Maybe ByteString)
hashPassword password = B.hashPasswordUsingPolicy B.slowerBcryptHashingPolicy $ TextE.encodeUtf8 password

-- Compare Passwords
comparePassword :: Text -> Text -> Bool
comparePassword userPassword password = B.validatePassword (TextE.encodeUtf8 password) (TextE.encodeUtf8 userPassword)
