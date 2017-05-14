{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}

module Types.User where

-- * Alternate Prelude
import           ClassyPrelude

-- * Composite ecosystem, and related, imports
import           Composite              ((:->), Record)
import           Composite.Aeson.Base   (JsonFormat, dimapJsonFormat,
                                         parseJsonWithFormat', toJsonWithFormat)
import           Composite.Aeson.Record (defaultJsonFormatRec, recJsonFormat)
import           Composite.Opaleye      (defaultRecTable)
import           Composite.TH           (withOpticsAndProxies)
import           Data.Aeson             (FromJSON, ToJSON, object, parseJSON,
                                         toJSON, withObject, (.:), (.=))
import           Data.Aeson.Types       (Parser, Value)
import           Data.Void              (Void)

-- * Opaleye, and related, imports
import           Data.UUID              (UUID)
import           Opaleye                (Column, Nullable, PGInt8, PGText,
                                         PGUuid, QueryArr, Table (Table),
                                         queryTable)

-- * Local module imports
import           Types.Common           (CCreatedAt, CCreatedAtMay, CUpdatedAt,
                                         CUpdatedAtMay, FCreatedAt,
                                         FCreatedAtMay, FUpdatedAt,
                                         FUpdatedAtMay)

--------------------------------------------------------------------------------

-- | User type definitions, types prefixed by 'F' denote native Haskell
-- representations, types prefixed by 'C' denote PostgreSQL representations
withOpticsAndProxies [d|
  type FUserId    = "id" :-> Int64
  type CUserId    = "id" :-> Column PGInt8
  type FUserIdMay = "id" :-> Maybe Int64
  type CUserIdMay = "id" :-> Maybe (Column PGInt8)
  -- ^ Auto-incrementing user ID.

  type FUserEmail    = "email" :-> Text
  type CUserEmail    = "email" :-> Column PGText
  type FUserEmailMay = "email" :-> Maybe Text
  -- ^ User email address.

  type FUserName    = "username" :-> Text
  type CUserName    = "username" :-> Column PGText
  type FUserNameMay = "username" :-> Maybe Text
  -- ^ Username.

  type FUserPassword    = "password" :-> Text
  type CUserPassword    = "password" :-> Column PGText
  type FUserPasswordMay = "password" :-> Maybe Text
  -- ^ User password, NOTE - ensure this is hashed.

  type FUserBioMay  = "bio" :-> Maybe Text
  type CUserBioNull = "bio" :-> Column (Nullable PGText)
  -- ^ User bio text, nullable, defaults to a NULL value.

  type FUserImageMay  = "image" :-> Maybe Text
  type CUserImageNull = "image" :-> Column (Nullable PGText)
  -- ^ User image URL, nullable, defaults to a NULL value.

  type FUserUUID    = "uuid" :-> UUID
  type CUserUUID    = "uuid" :-> Column PGUuid
  type FUserUUIDMay = "uuid" :-> Maybe UUID
  type CUserUUIDMay = "uuid" :-> Maybe (Column PGUuid)
  -- ^ User UUID, defaults to a PostgreSQL-generated UUID if none specified.

  type FUserToken = "token" :-> Text
  -- ^ User JWT authentication token.
  |]

--------------------------------------------------------------------------------

-- | Convenience function that wraps a @Composite@ JSON formatter in a single
-- member "user" JSON object.
wrapUserJson :: JsonFormat e a -> a -> Value
wrapUserJson fmt v = object [ "user" .= (toJsonWithFormat fmt) v]

-- | Convenience function that unwraps a singl a @Composite@ JSON parser in a single
-- member "user" JSON object.
unwrapUserJson :: JsonFormat Void a -> Value -> Parser a
unwrapUserJson fmt =
  withObject "User" (\o -> parseJsonWithFormat' fmt =<< o .: "user")

--------------------------------------------------------------------------------
-- | @Composite@ record of a 'User' login request and associated JSON parsing
-- and formatting instances.

type UserLogin = '[FUserEmail , FUserPassword]
newtype UserLoginJson = UserLoginJson { unUserLoginJson :: Record UserLogin }

userLoginJsonFormat :: JsonFormat e UserLoginJson
userLoginJsonFormat =
    dimapJsonFormat unUserLoginJson UserLoginJson $
      recJsonFormat defaultJsonFormatRec

instance FromJSON UserLoginJson where
  parseJSON = unwrapUserJson userLoginJsonFormat

instance ToJSON UserLoginJson where
  toJSON = wrapUserJson userLoginJsonFormat

--------------------------------------------------------------------------------
-- | @Composite@ record of a 'User' registration request and associated JSON
-- parsing and formatting instances.

type UserRegister = '[FUserEmail , FUserName , FUserPassword]

newtype UserRegisterJson
  = UserRegisterJson { unUserRegisterJson :: Record UserRegister }

userRegisterJsonFormat :: JsonFormat e UserRegisterJson
userRegisterJsonFormat =
    dimapJsonFormat unUserRegisterJson UserRegisterJson $
      recJsonFormat defaultJsonFormatRec

instance FromJSON UserRegisterJson where
  parseJSON = unwrapUserJson userRegisterJsonFormat

instance ToJSON UserRegisterJson where
  toJSON = wrapUserJson userRegisterJsonFormat

--------------------------------------------------------------------------------
-- | @Composite@ record of a 'User' update request and associated JSON parsing
-- and formatting instances.

type UserUpdate =
  '[ FUserEmailMay
   , FUserNameMay
   , FUserPasswordMay
   , FUserImageMay
   , FUserBioMay
   ]

newtype UserUpdateJson
  = UserUpdateJson { unUserUpdateJson :: Record UserUpdate }

userUpdateJsonFormat :: JsonFormat e UserUpdateJson
userUpdateJsonFormat =
    dimapJsonFormat unUserUpdateJson UserUpdateJson $
      recJsonFormat defaultJsonFormatRec

instance FromJSON UserUpdateJson where
  parseJSON = unwrapUserJson userUpdateJsonFormat

instance ToJSON UserUpdateJson where
  toJSON = wrapUserJson userUpdateJsonFormat

--------------------------------------------------------------------------------
-- | @Composite@ record of a 'User' response and associated JSON parsing and
-- formatting instances.

type UserResponse =
  '[ FUserEmail
   , FUserToken
   , FUserName
   , FUserBioMay
   , FUserImageMay
   ]

newtype UserResponseJson
  = UserResponseJson { unUserResponseJson :: Record UserResponse }

userResponseJsonFormat :: JsonFormat e UserResponseJson
userResponseJsonFormat =
    dimapJsonFormat unUserResponseJson UserResponseJson $
      recJsonFormat defaultJsonFormatRec

instance FromJSON UserResponseJson where
  parseJSON = unwrapUserJson userResponseJsonFormat

instance ToJSON UserResponseJson where
  toJSON = wrapUserJson userResponseJsonFormat

--------------------------------------------------------------------------------

-- | Haskell-level 'User' database view representation.
type UserView =
  '[ FUserId
   , FUserEmail
   , FUserName
   , FUserPassword
   , FUserBioMay
   , FUserImageMay
   , FUserUUID
   , FCreatedAt
   , FUpdatedAt
   ]

-- | Postgres-level 'User' database view representation.
type UserViewCols =
  '[ CUserId
   , CUserEmail
   , CUserName
   , CUserPassword
   , CUserBioNull
   , CUserImageNull
   , CUserUUID
   , CCreatedAt
   , CUpdatedAt
   ]

--------------------------------------------------------------------------------

-- | Haskell-level 'User' database write representation.
type UserInsert =
  '[ FUserIdMay
   , FUserEmail
   , FUserName
   , FUserPassword
   , FUserBioMay
   , FUserImageMay
   , FUserUUIDMay
   , FCreatedAtMay
   , FUpdatedAtMay
   ]

-- | Postgres-level 'User' database insert representation.
type UserInsertCols =
  '[ CUserIdMay
   , CUserEmail
   , CUserName
   , CUserPassword
   , CUserBioNull
   , CUserImageNull
   , CUserUUIDMay
   , CCreatedAtMay
   , CUpdatedAtMay
   ]

--------------------------------------------------------------------------------

-- | Opaleye "users" @Table@.
userTable :: Table (Record UserInsertCols) (Record UserViewCols)
userTable = Table "users" defaultRecTable

-- | @QueryArr@ selecting from the "users" table.
userQuery :: QueryArr () (Record UserViewCols)
userQuery = queryTable userTable
