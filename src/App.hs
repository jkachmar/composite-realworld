{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes       #-}
{-# LANGUAGE TemplateHaskell  #-}
{-# LANGUAGE TypeOperators    #-}

module App where

import           ClassyPrelude              hiding (Handler, keys)
import           Control.Monad.Logger       (askLoggerIO, logInfo)
import           Control.Monad.Trans.Maybe
import qualified Data.ByteString.Char8      as BS
import           Data.Pool                  (Pool)
import qualified Data.Pool                  as Pool
import           Database.PostgreSQL.Simple (Connection, close,
                                             connectPostgreSQL)
import           Network.Wai.Handler.Warp   (run)
import           Servant                    ((:~>) (NT), Handler, enter, serve)
import           System.Environment         (lookupEnv)

import           Api                        (api, server)
import           Foundation                 (AppStackM, Config (Config))
import           Logger                     (LogFunction, withLogger,
                                             withLoggingFn)

--------------------------------------------------------------------------------

-- | Initialize the application and serve the API.
startApp :: IO ()
startApp = do
  port    <- lookupSetting "APP_PORT" 8080
  connStr <- getConnStr
  let nConns = 5
  withLogger $ do
    withDbConnPool connStr nConns $ \ connPool -> do
      let config = Config connPool
      logFn <- askLoggerIO
      $logInfo $ "Starting server on port " <> tshow port
      liftIO . run port . serve api $ enter (appStackToHandler config logFn) server

--------------------------------------------------------------------------------

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
    Nothing -> throwIO (userError "Database configuration not present in environment!")
    Just str -> pure str
