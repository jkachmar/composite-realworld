module Logger (LogFunction, withLogger, withLoggingFn) where

import           ClassyPrelude
import           Control.Monad.Logger  (Loc (loc_filename, loc_module, loc_package, loc_start),
                                        LogLevel, LogSource,
                                        LoggingT (runLoggingT))
import           Data.ByteString.Char8 (hPutStrLn)
import           System.IO             (stderr)
import           System.Log.FastLogger (LogStr, ToLogStr (toLogStr), fromLogStr)

--------------------------------------------------------------------------------

-- | An alias for the type that @LoggingT@ requires to log messages
type LogFunction = Loc -> LogSource -> LogLevel -> LogStr -> IO ()

--------------------------------------------------------------------------------

-- | Create a logging context using the default logging settings
withLogger :: (MonadMask m, MonadIO m) => LoggingT m a -> m a
withLogger action = runLoggingT action logMsg

-- | Create a logging context using the given @LogFunction@
withLoggingFn :: LogFunction -> LoggingT m a -> m a
withLoggingFn logFn logger = runLoggingT logger logFn

--------------------------------------------------------------------------------

-- | Return the current time in a logging-friendly format (i.e. a @LogStr@)
nowLogString :: IO LogStr
nowLogString = do
  now <- getCurrentTime
  pure . toLogStr $ formatTime defaultTimeLocale "%Y-%m-%d %T%Q" now

--------------------------------------------------------------------------------

-- | Given a code location (i.e. a @Loc@), produce a formatted @LogStr@
locLogString :: Loc -> LogStr
locLogString loc =
  let package      = toLogStr . loc_package $ loc
      module'      = toLogStr . loc_module $ loc
      filename     = toLogStr . loc_filename $ loc
      linePosition = toLogStr . show . fst . loc_start $ loc
      charPosition = toLogStr . show . snd . loc_start $ loc

  in  package <> ":" <> module' <> " " <> filename <> ":" <> linePosition <> ":" <> charPosition

--------------------------------------------------------------------------------

-- | Log some @LogStr@ for a given @Loc@ and @LogLevel@
logMsg :: LogFunction
logMsg loc _ level msg = do
  dateLogStr <- nowLogString
  let logStr = dateLogStr
        <> " [" <> (toLogStr . show) level <> "] "
        <> msg <> " @(" <> locLogString loc <> ")"
  (hPutStrLn stderr . fromLogStr) logStr
