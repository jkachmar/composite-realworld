{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}

module Api.Users where

import           ClassyPrelude

import           Servant
import           Servant.Auth.Server

import           Foundation          (AppStackM)
import           Types.Auth          (UserId)
import           Types.User          (UserApiLoginJson, UserApiRegJson,
                                      UserApiRespJson)

-- n.b. I don't really know the accepted style to write Servant types ¯\_(ツ)_/¯

-- | Servant type-level representation of the "users" route fragment
type UsersAPI auths = (Auth auths UserId :> ProtectedAPI) :<|> UnprotectedAPI

usersServer :: ServerT (UsersAPI auths) AppStackM
usersServer = protected :<|> unprotected

--------------------------------------------------------------------------------

-- | Type representation for routes that require authentication.
type ProtectedAPI =
  "users"
    :> ReqBody '[JSON] UserApiRegJson
      :> Post '[JSON] UserApiRespJson

-- | Endpoint dispatcher for protected routes, handles authentication.
protected :: AuthResult UserId -> ServerT ProtectedAPI AppStackM
protected (Authenticated _) = register
protected _                 = throwAll err401

-- | Registration endpoint, protected by the authentication handlier.
register :: UserApiRegJson -> AppStackM UserApiRespJson
register = undefined

--------------------------------------------------------------------------------

-- | Type alias for the login response with a secure cookie
type UserApiRespJsonAndCookie = Headers '[ Header "Set-Cookie" SetCookie
                                         , Header "Set-Cookie" SetCookie
                                         ] UserApiRespJson

-- | Type representation for routes that do not require authentication.
type UnprotectedAPI =
  "users"
    :> "login"
      :> ReqBody '[JSON] UserApiLoginJson
        :> Post '[JSON] UserApiRespJson

-- | Endpoint dispatcher for unprotected routes.
unprotected :: ServerT UnprotectedAPI AppStackM
unprotected = login

-- | Registration endpoint, protected by the authentication handlier.
login :: UserApiLoginJson -> AppStackM UserApiRespJson
login = undefined
