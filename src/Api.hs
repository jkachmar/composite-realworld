{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators   #-}

module Api where

import           ClassyPrelude
import           Control.Monad.Logger (logInfo)
import           Data.Proxy           (Proxy (Proxy))
import           Foundation           (AppStackM)
import           Servant              ((:>), Get, JSON, ServerT)

--------------------------------------------------------------------------------

type API = "root" :> Get '[JSON] ()

api :: Proxy API
api = Proxy

server :: ServerT API AppStackM
server = do
  $logInfo "thing happened"
  pure ()
