module Cmd
       (
          -- Functions
          getApiKey
       ) where

import Model (ApiKey(..))

import Data.String (IsString, fromString)
import System.Environment (getEnv)


--- SLATE_API_KEY=a3AsVcKREaGCrnfspLknQW4ps

getApiKey :: IO ApiKey
getApiKey = ApiKey <$> fromSystemEnv "SLATE_API_KEY"

fromSystemEnv :: IsString a => String -> IO a
fromSystemEnv key = fromString <$> getEnv key