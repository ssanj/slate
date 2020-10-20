module Cmd
       (
          -- Functions
          getApiKey
       ) where

import Model (ApiKey(..))

import Data.String (IsString, fromString)
import System.Environment (getEnv)

getApiKey :: IO ApiKey
getApiKey = ApiKey <$> fromSystemEnv "SLATE_API_KEY"

fromSystemEnv :: IsString a => String -> IO a
fromSystemEnv key = fromString <$> getEnv key