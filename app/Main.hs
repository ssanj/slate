{-# LANGUAGE OverloadedStrings #-}

module Main where

import SlateApi (server)
import Model    (SlateConfig(..), SlateDatabaseConfig(..))
import Cmd      (getApiKey)

main :: IO ()
main = config >>= server

config :: IO SlateConfig
config = do
  apiKey <- getApiKey
  let dbConfig = SlateDatabaseConfig "db/scrib.db"
  pure $ SlateConfig apiKey dbConfig