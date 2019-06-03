{-# LANGUAGE DataKinds, DeriveGeneric, DuplicateRecordFields, OverloadedStrings, TypeOperators #-}

module Types
          ( User(..)
          , Login(..)
          , Register(..)
          , Response(..)
          , PublicUser(..)
          , TokenBody(..)
          , UserBody(..)
          ) where

import           Data.Aeson as A (FromJSON, ToJSON, Value (..), decode, encode)
import           Data.Text (Text)
import           Data.UUID (UUID (..))
import           GHC.Generics (Generic)

data User = User {
  id        :: UUID,
  email     :: Text,
  firstName :: Text,
  lastName  :: Text,
  password  :: Text
} deriving (Eq, Show, Generic)

instance ToJSON User
instance FromJSON User

data Login = Login{
  email    :: Text,
  password :: Text
} deriving (Eq, Show, Generic)

instance ToJSON Login
instance FromJSON Login


data Register = Register{
  email     :: Text,
  firstName :: Text,
  lastName  :: Text,
  password  :: Text
} deriving (Eq, Show, Generic)

instance ToJSON Register
instance FromJSON Register


data Response = Response {
    message :: Text,
    success :: Bool,
    token   :: Maybe Text,
    user    :: Maybe PublicUser
  } deriving (Eq, Show, Generic)

instance ToJSON Response
instance FromJSON Response

data PublicUser = PublicUser {
  id        :: UUID,
  email     :: Text,
  firstName :: Text,
  lastName  :: Text
} deriving (Eq, Show, Generic)

instance ToJSON PublicUser
instance FromJSON PublicUser

newtype TokenBody = TokenBody {
  user :: PublicUser
} deriving (Eq, Show, Generic)

instance ToJSON TokenBody
instance FromJSON TokenBody


newtype UserBody = UserBody {
  jwt :: Text
} deriving (Eq, Show, Generic)

instance ToJSON UserBody
instance FromJSON UserBody
