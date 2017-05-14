{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell  #-}

module BCrypt where

import           ClassyPrelude         hiding (hash)
import qualified Crypto.KDF.BCrypt     as BC

--------------------------------------------------------------------------------

-- | Newtype to define password hashing and validation functions against.
newtype BCrypt
  = BCrypt
  { unBCrypt :: Text
  } deriving (Eq, Read, Show)

-- | Hash a given password using the bcrypt algorithm using the `2b` version.
hashPassword :: MonadBaseControl IO m => Text -> m BCrypt
hashPassword pass = BCrypt . decodeUtf8 <$> hashPassword' pass
  where
    hashPassword' :: MonadBaseControl IO m => Text -> m ByteString
    hashPassword' = (liftBase . BC.hashPassword 12) . encodeUtf8
