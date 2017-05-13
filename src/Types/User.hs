{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}

module Types.User where

import           ClassyPrelude
import           Control.Lens.TH    (makeWrapped)
import           Data.UUID          (UUID)

import           Composite          ((:->), Record)
import           Composite.Aeson.TH (makeRecJsonWrapper)
import           Composite.Opaleye  (defaultRecTable)
import           Composite.TH       (withOpticsAndProxies)

import           Opaleye            (Column, Nullable, PGInt8, PGText, PGUuid,
                                     QueryArr, Table (Table), queryTable)

import           Types.Common       (CCreatedAt, CCreatedAtMay, CUpdatedAt,
                                     CUpdatedAtMay, FCreatedAt, FCreatedAtMay,
                                     FUpdatedAt, FUpdatedAtMay)

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

  type FUserBioMay = "bio" :-> Maybe Text
  type CUserBio    = "bio" :-> Column (Nullable PGText)
  type CUserBioMay = "bio" :-> Maybe (Column (Nullable PGText))
  -- ^ User bio text, nullable, defaults to a NULL value.

  type FUserImageMay = "image" :-> Maybe Text
  type CUserImage    = "image" :-> Column (Nullable PGText)
  type CUserImageMay = "image" :-> Maybe (Column (Nullable PGText))
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

-- | @Composite@ record of a 'User' login request.
type UserLogin = '[FUserEmail , FUserPassword]

makeRecJsonWrapper "UserLoginJson" ''UserLogin
makeWrapped ''UserLoginJson

-- | @Composite@ record of a 'User' registration request.
type UserRegister = '[FUserEmail , FUserName , FUserPassword]

makeRecJsonWrapper "UserRegisterJson" ''UserRegister
makeWrapped ''UserRegisterJson

-- | @Composite@ record of a 'User' update request.
type UserUpdate =
  '[ FUserEmailMay
   , FUserNameMay
   , FUserPasswordMay
   , FUserImageMay
   , FUserBioMay
   ]

makeRecJsonWrapper "UserUpdateJson" ''UserUpdate
makeWrapped ''UserUpdateJson

-- | @Composite@ record of a 'User' response.
type UserResponse =
  '[ FUserEmail
   , FUserToken
   , FUserName
   , FUserBioMay
   , FUserImageMay
   ]

makeRecJsonWrapper "UserResponseJson" ''UserResponse
makeWrapped ''UserResponseJson

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

--------------------------------------------------------------------------------

-- | Postgres-level 'User' database view representation.
type UserViewCols =
  '[ CUserId
   , CUserEmail
   , CUserName
   , CUserPassword
   , CUserBio
   , CUserImage
   , CUserUUID
   , CCreatedAt
   , CUpdatedAt
   ]

-- | Postgres-level 'User' database insert representation.
type UserInsertCols =
  '[ CUserIdMay
   , CUserEmail
   , CUserName
   , CUserPassword
   , CUserBioMay
   , CUserImageMay
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
