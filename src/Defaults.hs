{-# LANGUAGE OverloadedStrings #-}

module Defaults (middleware, database, errorHandler, staticFileDir) where

import Model (MiddlewareType(..), SlateDatabaseConfig(..), SlateErrorHandler(..), StaticFileDir(..))

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

staticFileDir :: StaticFileDir
staticFileDir = "./static"