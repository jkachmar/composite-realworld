{-# LANGUAGE TemplateHaskell #-}

module Types.BCrypt where

import           ClassyPrelude         hiding (hash)
import           Control.Monad.Logger  (MonadLogger, logError)
import qualified Crypto.KDF.BCrypt     as BC

--------------------------------------------------------------------------------

-- | Newtype to define password hashing and validation functions against.
newtype BCrypt
  = BCrypt
  { unBCrypt :: Text
  } deriving (Eq, Read, Show)

-- | Hash a given password using the bcrypt algorithm using the `2b` version.
hashPassword :: MonadIO m => Text -> m BCrypt
hashPassword pass = BCrypt . decodeUtf8 <$> hashPassword' pass
  where
    hashPassword' :: MonadIO m => Text -> m ByteString
    hashPassword' = (liftIO . BC.hashPassword 12) . encodeUtf8

-- | Attempt to validate a password hashed with BCrypt against the `2b`, `2a`, or
-- `2y` version prefixes (per `cryptonite` defaults); if validation fails (e.g.
-- invalid hash or unsupported hashing version), an error is logged using the
-- provided @MonadLogger@ interface.
validatePassword :: MonadLogger m => BCrypt -> Text -> m Bool
validatePassword hash pass = do
  let hash'   = encodeUtf8 . unBCrypt $ hash
      pass'   = encodeUtf8 pass
      isValid = BC.validatePasswordEither hash' pass'

  case isValid of
    Left err -> do
      $logError $ "Password validation failed with: " <> (pack err)
      pure False
    Right v -> pure v
