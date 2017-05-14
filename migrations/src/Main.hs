{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE QuasiQuotes         #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}

module Main where

import           ClassyPrelude              hiding (fromList, lookup, pack,
                                             unpack)
import           Prelude                    (read)

import           Configuration.Dotenv       (onMissingFile, parseFile)
import           Data.Map.Strict            (fromList, lookup)
import           Data.Text                  (pack, unpack)
import           Database.PostgreSQL.Simple ()
import           Refurb                     (ConnInfo (..), Migration,
                                             MonadMigration, execute_, qqSql,
                                             refurbMain, schemaMigration)

--------------------------------------------------------------------------------

main :: IO ()
main =
  refurbMain
    loadConfig
    migrations

--------------------------------------------------------------------------------

loadConfig :: FilePath -> IO ConnInfo
loadConfig configFile = do

  envs <- fromList <$> parseFile configFile
  case (getConnInfo envs) of
    Nothing       -> throwIO $ userError "Unable to read connection info!"
    Just connInfo -> pure connInfo

  where
    getConnInfo :: Map String String -> Maybe ConnInfo
    getConnInfo envs = do
      connHost     <- pack <$> lookup "PGHOST"     envs
      connPort     <- read <$> lookup "PGPORT"     envs
      connUser     <- pack <$> lookup "PGUSER"     envs
      connPassword <- pack <$> lookup "PGPASS"     envs
      connDbName   <- pack <$> lookup "PGDATABASE" envs
      pure $ ConnInfo{..}

--------------------------------------------------------------------------------

migrations :: [Migration]
migrations =
  [ schemaMigration "public" "create-users-table"    createUsersTable
  ]

--------------------------------------------------------------------------------

createUsersTable :: MonadMigration m => m ()
createUsersTable =
  let createUsersTblQ =
        [qqSql|
          CREATE TABLE public.users
            ( id          SERIAL4              PRIMARY KEY UNIQUE
            , email       TEXT        NOT NULL UNIQUE
            , username    TEXT        NOT NULL
            , password    TEXT        NOT NULL
            , bio         TEXT                 DEFAULT NULL
            , image       TEXT                 DEFAULT NULL
            , created_at  TIMESTAMPTZ NOT NULL DEFAULT CURRENT_TIMESTAMP
            , updated_at  TIMESTAMPTZ NOT NULL DEFAULT CURRENT_TIMESTAMP
            , uuid        UUID        NOT NULL DEFAULT uuid_generate_v4()
            );
        |]
  in void $ execute_ createUsersTblQ
