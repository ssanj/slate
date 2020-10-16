module Main where

import SlateApi (server)

import Cmd (getApiKey)

main :: IO ()
main = getApiKey >>= server
