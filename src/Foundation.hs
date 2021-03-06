{-# LANGUAGE FlexibleContexts #-}

module Foundation where

import           ClassyPrelude                        hiding (Handler)
import           Control.Monad.Logger                 (LoggingT)
import           Data.Pool                            (Pool, withResource)
import           Database.PostgreSQL.Simple           (Connection,
                                                       withTransaction)
import           Network.Wai                          (Middleware)
import           Network.Wai.Middleware.RequestLogger (logStdout, logStdoutDev)
import           Servant                              (Handler)
import           Servant.Auth.Server                  (JWTSettings)

--------------------------------------------------------------------------------

-- | An alias for the monadic context in which all requests are handled;
-- @LoggingT@ provides access to the logging context, and @ReaderT Config@
-- provides access the @Config@ record.
type AppStackM = ReaderT Config (LoggingT Handler)

-- | A record containing the application's configuration information (n.b. for
-- now, this only contains our database connection pool, and JWT settings).
data Config
  = Config
  { getConnPool    :: Pool Connection
  , getJWTSettings :: JWTSettings
  }

--------------------------------------------------------------------------------

-- | Execute a database action within a context compatible with @AppStackM@.
withDb :: (MonadBaseControl IO m, MonadReader Config m)
       => (Connection -> IO a) -> m a
withDb action = do
  pool <- asks getConnPool
  withDbConn pool action

-- | Execute a database action within a context compatible with @AppStackM@ and
-- a PostgreSQL transactional context.
withDbTransaction :: (MonadBaseControl IO m, MonadReader Config m)
                  => (Connection -> IO a) -> m a
withDbTransaction action = do
  pool <- asks getConnPool
  withDbTransactionConn pool action

-- | Execute a database action with a database connection taken from the pool.
withDbConn :: MonadBaseControl IO m
           => Pool Connection -> (Connection -> IO a)
           -> m a
withDbConn pool action = liftBase $ withResource pool action

-- | Execute a database action with a database connection taken from the pool
-- and as PostgreSQL transactional context.
withDbTransactionConn :: MonadBaseControl IO m
                      => Pool Connection -> (Connection -> IO a)
                      -> m a
withDbTransactionConn pool action =
  withDbConn pool $ \ conn ->
    withTransaction conn (action conn)

--------------------------------------------------------------------------------

data Environment
  = DEV
  | TEST
  | PROD
  deriving (Eq, Read, Show)

setLoggingMiddleware :: Environment -> Middleware
setLoggingMiddleware TEST = id
setLoggingMiddleware DEV  = logStdoutDev
setLoggingMiddleware PROD = logStdout
