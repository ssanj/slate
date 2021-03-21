{-# LANGUAGE OverloadedStrings #-}

module Main where

import Defaults

import SlateApi (server)
import Model    (SlateConfig(..), SlateDatabaseConfig(..), ApiKey)
import Cmd      (getApiKey)

main :: IO ()
main = config >>= server

config :: IO SlateConfig
config = withDefaults <$> getApiKey

withDefaults :: ApiKey -> SlateConfig
withDefaults apiKey = SlateConfig apiKey Defaults.database Defaults.middleware Defaults.errorHandler Defaults.staticFileDir