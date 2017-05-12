{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}

module Types.Common where

import           ClassyPrelude

import           Composite          ((:->))
import           Composite.TH       (withOpticsAndProxies)

import           Opaleye            (Column, PGTimestamptz)

--------------------------------------------------------------------------------

-- | Record creation and update timestamp definitions, types prefixed by 'F'
-- denote native Haskell representations, types prefixed by 'C' denote
-- PostgreSQL representations
withOpticsAndProxies [d|
  type FCreatedAt    = "created_at" :-> UTCTime
  type CCreatedAt    = "created_at" :-> Column PGTimestamptz
  type FCreatedAtMay = "created_at" :-> Maybe UTCTime
  type CCreatedAtMay = "created_at" :-> Maybe (Column PGTimestamptz)
  -- ^ Record creation timestamp.

  type FUpdatedAt    = "updated_at" :-> UTCTime
  type CUpdatedAt    = "updated_at" :-> Column PGTimestamptz
  type FUpdatedAtMay = "updated_at" :-> Maybe UTCTime
  type CUpdatedAtMay = "updated_at" :-> Maybe (Column PGTimestamptz)
  -- ^ Record update timestamp.
  |]
