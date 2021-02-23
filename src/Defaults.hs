{-# LANGUAGE OverloadedStrings #-}

module Defaults (middleware, database, errorHandler) where

import Model (MiddlewareType(..), SlateDatabaseConfig(..), SlateErrorHandler(..))

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

errorHandler :: Maybe SlateErrorHandler
errorHandler = Just SlateErrorHandler