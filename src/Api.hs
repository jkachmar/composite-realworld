{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators   #-}

module Api where

import           Foundation          (AppStackM)
import           Servant             ((:>), Proxy (Proxy), ServerT)
import           Servant.Auth.Server (JWT)

import           Api.Users

--------------------------------------------------------------------------------

type Api auths = "api" :> (UsersApi auths)

api :: Proxy (Api '[JWT])
api = Proxy

server :: ServerT (Api auths) AppStackM
server = usersServer
