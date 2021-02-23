{-# LANGUAGE OverloadedStrings #-}

module Defaults (middleware, database) where

import Model (MiddlewareType(..), SlateDatabaseConfig(..))

middleware :: [MiddlewareType]
middleware =
  [
    GZipping
  , Logging
  , StaticFileServing
  , ApiKeyRequiring
  ]

database :: SlateDatabaseConfig
database = SlateDatabaseConfig "db/scrib.db"

