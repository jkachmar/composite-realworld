{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes       #-}
{-# LANGUAGE TemplateHaskell  #-}
{-# LANGUAGE TypeOperators    #-}

module App where

import           ClassyPrelude              hiding (Handler, keys)
import           Configuration.Dotenv       (loadFile)
import           Control.Monad.Logger       (askLoggerIO, logInfo)
import           Control.Monad.Trans.Maybe  (MaybeT (MaybeT), runMaybeT)
import           Crypto.JOSE.JWK
import qualified Data.ByteString.Char8      as BS
import           Data.Pool                  (Pool)
import qualified Data.Pool                  as Pool
import           Data.X509                  (PrivKey (PrivKeyDSA, PrivKeyRSA))
import           Data.X509.File             (readKeyFile)
import           Database.PostgreSQL.Simple (Connection, close,
                                             connectPostgreSQL)
import           Network.Wai.Handler.Warp   (run)
import           Servant                    ((:~>) (NT), Handler, enter, serve)
import           Servant.Auth.Server        (defaultJWTSettings)
import           System.Environment         (lookupEnv)

import           Api                        (api, server)
import           Foundation                 (AppStackM, Config (Config),
                                             Environment (..),
                                             setLoggingMiddleware)
import           Logger                     (LogFunction, withLogger,
                                             withLoggingFn)

--------------------------------------------------------------------------------

-- | Initialize the application and serve the API.
startApp :: IO ()
startApp = do
  loadFile False ".env"
  -- Load settings from a ".env" file, if present; do not override existing env vars

  env <- lookupSetting "ENV" DEV

  port <- lookupSetting "APP_PORT" 8080
  -- Get the specified application port, default to 8080 if "APP_PORT" not present

  connStr <- getConnStr
  -- Build a connection string from env vars, throw IO failure if env vars not present

  nConns <- lookupSetting "DB_CONNS" 5
  -- Get the specified number of DB connections, default to 5 if "DB_CONNS" not present

  rsaKey <- lookupSetting "RSA_KEY" (throwIO $ userError "'KEYPATH' environment variable not set!")
  jwtCfg <- defaultJWTSettings <$> mkJWK rsaKey
  -- Generate JWTSettings from an RSA private keyfile

  withLogger $ do
    withDbConnPool connStr nConns $ \ connPool -> do
      logFn <- askLoggerIO
      let config    = Config connPool jwtCfg
          reqLogger = setLoggingMiddleware env
          service   = enter (appStackToHandler config logFn) server

      $logInfo $ "Starting server on port " <> tshow port

      liftIO . run port . reqLogger . serve api $ service

--------------------------------------------------------------------------------

-- | Creates a JSON Web Key from the first RSA key in the keyfile at the given
-- @FilePath@; throws an IO error if no key is present or the first key in the
-- keyfile is a DSA key.
mkJWK :: FilePath -> IO JWK
mkJWK keypath = do
  maybePk <- readKeyFile keypath
  case (headMay maybePk) of
    Nothing -> throwIO $ userError $ "No valid keys present at [" <> show keypath <> "]"
    Just (PrivKeyDSA _) -> throwIO $ userError $ "Invalid key (DSA) present in [" <> show keypath <> "]"
    Just (PrivKeyRSA pk) -> pure $ fromRSA pk

-- | Looks up an environment variable, given a default to fall back to, and
-- `read`s that variable into an inferred type.
lookupSetting :: Read a => String -> a -> IO a
lookupSetting env def = do
  maybeEnv <- lookupEnv env
  case maybeEnv of
    Nothing -> pure def
    Just str -> maybe (handleFailure str) pure (readMay str)
  where
    handleFailure str = error $ mconcat
      [ "Failed to read [["
      , str
      , "]] for environment variable "
      , env
      ]

--------------------------------------------------------------------------------

-- | Convert an @AppStackM@ into a Servant @Handler@ for a given @Config@ and
-- @LogFunction@.
appStackToHandler :: Config -> LogFunction -> (AppStackM :~> Handler)
appStackToHandler config logger = NT convAppStack
  where
    convAppStack action =
      withLoggingFn logger $ runReaderT action config

--------------------------------------------------------------------------------

-- | Newtype wrapper representing a PostgreSQL connection string.
newtype ConnStr = ConnStr { unConnStr :: ByteString } deriving Show

-- | Create a pool of database connections and run the given action with it,
-- typically during server initialization.
withDbConnPool :: MonadBaseControl IO m
               => ConnStr -> Int
               -> (Pool Connection -> m a)
               -> m a
withDbConnPool connStr nConns action = do
  stm <- liftBaseWith $ \ runInBase ->
    bracket createPool Pool.destroyAllResources (runInBase . action)
  restoreM stm
  where
    createPool = Pool.createPool (connectPostgreSQL bsConnStr) close 1 20 nConns
    bsConnStr = unConnStr connStr

-- | Make a @ConnString@ from environment variables, throwing an error if any
-- cannot be found.
getConnStr :: IO ConnStr
getConnStr = do
  connStr <- runMaybeT $ do
    let keys = [ "host="
               , " port="
               , " user="
               , " password="
               , " dbname="
               ]
        envs = [ "PGHOST"
               , "PGPORT"
               , "PGUSER"
               , "PGPASS"
               , "PGDATABASE"
               ]
    envVars <- traverse (MaybeT . lookupEnv) envs
    pure . ConnStr . mconcat . zipWith (<>) keys $ BS.pack <$> envVars
  case connStr of
    Nothing -> throwIO $ userError $ "Database configuration not present in environment!"
    Just str -> pure str
