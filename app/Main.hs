{-# LANGUAGE OverloadedStrings #-}

module Main where

import Defaults

import SlateApi (server)
import Model    (SlateConfig(..), SlateDatabaseConfig(..))
import Cmd      (getApiKey)

main :: IO ()
main = config >>= server

config :: IO SlateConfig
config =
  (\apiKey ->
      SlateConfig apiKey Defaults.database Defaults.middleware
  ) <$> getApiKey