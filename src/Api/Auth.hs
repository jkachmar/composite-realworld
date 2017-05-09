{-# LANGUAGE DeriveGeneric #-}

module Api.Auth where

import           ClassyPrelude
import           Data.Aeson
import           Servant       ()
import           Servant.Auth.Server

-------------------------------------------------------------------------------

newtype AuthUser
  = AuthUser
  { userId  :: Text
  } deriving (Eq, Show, Read, Generic)

instance FromJSON AuthUser
instance FromJWT AuthUser
instance ToJSON AuthUser
instance ToJWT AuthUser
