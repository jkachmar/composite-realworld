{-# LANGUAGE DeriveGeneric   #-}

module Types.Auth where

import           ClassyPrelude
import           Data.Aeson          (FromJSON, ToJSON, object, parseJSON,
                                      toJSON, withObject, (.:), (.=))
import           Data.UUID           (UUID, fromText, toText)
import           Servant.Auth.Server (FromJWT, ToJWT)

-------------------------------------------------------------------------------

newtype UserId = UserId { unUserId :: UUID } deriving (Eq, Read, Show)

instance FromJSON UserId where
  parseJSON = withObject "user authentication JWT" $ \o -> do
     maybeUid <- o .: "user_uuid"
     case (fromText maybeUid) of
       Nothing  -> fail $ "unable to parse field [" <> unpack maybeUid <> "] int"
       Just uId -> pure . UserId $ uId

instance ToJSON UserId where
  toJSON (UserId uId) = object [ "user_uuid" .= (toText uId) ]

instance FromJWT UserId
instance ToJWT   UserId
