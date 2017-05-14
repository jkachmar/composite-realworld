{-# LANGUAGE DeriveGeneric   #-}

module Types.Auth where

import           ClassyPrelude
import           Data.Aeson          (FromJSON, ToJSON, object, parseJSON,
                                      toJSON, withObject, (.:), (.=))
import           Data.UUID           (UUID, fromText, toText)
import           Servant.Auth.Server (FromJWT, ToJWT)

-------------------------------------------------------------------------------

newtype Token = Token { unToken :: UUID } deriving (Eq, Read, Show)

instance FromJSON Token where
  parseJSON = withObject "user authentication JWT" $ \o -> do
     maybeTok <- o .: "user_uuid"
     case (fromText maybeTok) of
       Nothing  -> fail $ "unable to parse field [" <> unpack maybeTok <> "]"
       Just tok -> pure . Token $ tok

instance ToJSON Token where
  toJSON (Token tok) = object [ "user_uuid" .= (toText tok) ]

instance FromJWT Token
instance ToJWT   Token

-------------------------------------------------------------------------------

newtype JWTText = JWTText { unJWTText :: Text } deriving (Eq, Read, Show)
