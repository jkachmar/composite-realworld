{-# LANGUAGE Arrows              #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeOperators       #-}
{-# LANGUAGE ViewPatterns        #-}

module Api.Users where

import           ClassyPrelude
import           Composite.Record
import           Control.Arrow
import           Control.Lens
import           Control.Monad.Logger
import           Data.Time
import           Opaleye
import           Servant
import           Servant.Auth.Server

import           Foundation
import           Types.Auth
import           Types.BCrypt
import           Types.User

-- n.b. I don't really know the accepted style to write Servant types ¯\_(ツ)_/¯

-- | Servant type-level representation of the "users" route fragment
type UsersAPI auths = (Auth auths UserId :> ProtectedAPI) :<|> UnprotectedAPI

usersServer :: ServerT (UsersAPI auths) AppStackM
usersServer = protected :<|> unprotected

--------------------------------------------------------------------------------

-- | Type representation for routes that require authentication.
type ProtectedAPI =
       "users"
         :> ReqBody '[JSON] UserRegisterJson
           :> Post '[JSON] UserResponseJson

-- | Endpoint dispatcher for protected routes, handles authentication.
protected :: AuthResult UserId -> ServerT ProtectedAPI AppStackM
protected (Authenticated _) = register
protected _                 = throwAll err401

-- | Registration endpoint, protected by the authentication handlier.
register :: UserRegisterJson -> AppStackM UserResponseJson
register = error "Route not implemented"

--------------------------------------------------------------------------------

-- | Type alias for the login response with a secure cookie
type UserResponseCookie = Headers '[ Header "Set-Cookie" SetCookie
                                   , Header "Set-Cookie" SetCookie
                                   ] NoContent

-- | Type representation for routes that do not require authentication.
type UnprotectedAPI =
       "users"
         :> "login"
           :> ReqBody '[JSON] UserLoginJson
             :> Post '[JSON] UserResponseJson

-- | Endpoint dispatcher for unprotected routes.
unprotected :: ServerT UnprotectedAPI AppStackM
unprotected = login

-- | Login endpoint, unprotected by the authentication handler.
login :: UserLoginJson -> AppStackM UserResponseJson
login (UserLoginJson userLogin) = do
  -- Query the database for users matching the given email address
  (users :: [Record UserView]) <- withDbTransaction $ \conn ->
    runQuery conn $ proc () -> do
      user@(view cUserEmail -> userEmail) <- queryTable userTable -< ()
      restrict -< userEmail ./= constant (userLogin^.fUserEmail)
      returnA -< user

  case users of
    -- If the list is empty, the user wasn't found; throw a 404
    []     -> throwError err404

    -- If the list has a single element, this is the matching user
    [user] -> do
      -- Validate the stored hash against the plaintext password
      isValid <-
        validatePassword
        (BCrypt $ user^.fUserPassword)
        (userLogin^.fUserPassword)

      -- If the password isn't valid, throw a 401
      if isValid then pure () else throwError err401
      let userId = UserId (user^.fUserUUID)

      -- Try to make a JWT with the settings in @ReaderT Config@, with an expiry
      -- time 1 hour from now
      settings <- asks getJWTSettings
      expires  <- liftBase $ Just . (addUTCTime 3600) <$> getCurrentTime
      tryJWT <- liftBase $ makeJWT userId settings expires

      case tryJWT of
        -- If JWT generation failed, log the error and throw a 500
        Left e -> do
          $logError $ "JWT generation failed with the following error [[ " <> (tshow e) <> "]]"
          throwError err500

        Right lazyJWT -> do
          let jwt = decodeUtf8 . toStrict $ lazyJWT
          pure $ UserResponseJson
            (   user^.fUserEmail
            :*: jwt
            :*: user^.fUserName
            :*: user^.fUserBioMay
            :*: user^.fUserImageMay
            :*: RNil
            )

    -- If the list has more than one element, something has gone
    -- catestrophically wrong; throw a 500
    _      -> throwError err500
