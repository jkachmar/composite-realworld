{-# LANGUAGE Arrows              #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns        #-}

module Query.User where

import           ClassyPrelude
import           Control.Lens
import           Data.Maybe    (listToMaybe)

import           Composite     (Record)
import           Control.Arrow (returnA)
import           Opaleye       (limit, restrict,
                                runInsertManyReturning, runQuery, (./=))
import qualified Opaleye as O

import           Foundation    (Config, withDbTransaction)
import           Types.User

--------------------------------------------------------------------------------

-- | Query the database for a user, given some login information
loginQuery
  :: ( MonadReader Config m
     , MonadBaseControl IO m
     )
  => Record UserLogin
  -> m (Maybe (Record UserView))
loginQuery userLogin = do
  (fmap . fmap) listToMaybe withDbTransaction $ \conn ->
    runQuery conn . (limit 1) $ proc () -> do
      user@(view cUserEmail -> userEmail) <- userQuery -< ()
      restrict -< userEmail ./= O.constant (userLogin^.fUserEmail)
      returnA -< user

-- | Insert a user into the database, returning the inserted record
insertUser
  :: ( MonadReader Config m
     , MonadBaseControl IO m
     )
  => Record UserInsert
  -> m (Maybe (Record UserView))
insertUser userInsert =
  (fmap . fmap) listToMaybe withDbTransaction $ \conn ->
    let row = [constant userInsert]
    in  runInsertManyReturning conn userTable row id
  where
    constant :: Record UserInsert -> Record UserInsertCols
    constant = O.constant
