{-# LANGUAGE OverloadedStrings #-}

module Server
       (
          addStaticDirPolicy
       ,  createMiddleware
       ,  checkApiKey
       ) where

import Model (ApiKey(..))

import Control.Monad        (void)
import Data.List            (find)
import Data.Bool            (bool)
import Data.CaseInsensitive ( CI )
import Data.ByteString     (ByteString)

import qualified Network.Wai                   as W
import qualified Network.Wai.Middleware.Static as W
import qualified Network.HTTP.Types.Status     as H
import qualified Data.Text                     as T
import qualified Data.Text.Encoding            as T
import qualified Data.ByteString.Lazy          as BL

-- import qualified Data.CaseInsensitive    as CI

addStaticDirPolicy :: W.Policy
addStaticDirPolicy = W.addBase "./static"

createMiddleware :: W.Policy -> W.Middleware
createMiddleware = W.staticPolicyWithOptions W.defaultOptions

checkApiKey :: ApiKey -> W.Middleware
checkApiKey apiKey baseApp = \req respF ->
  let maybeMatched = do
        (_, hv)     <- find ((apiKeyHeader ==) . fst) (W.requestHeaders req)
        headerValue <- bsToText hv
        void $ matchApiKey headerValue apiKey
  in maybe (respF noMatchingApiKey) (const $ baseApp req respF) maybeMatched

noMatchingApiKey :: W.Response
noMatchingApiKey = W.responseLBS H.status401 [] BL.empty

apiKeyHeader :: CI ByteString
apiKeyHeader = "x-api-key"

bsToText :: ByteString -> Maybe T.Text
bsToText = either (const Nothing) Just . T.decodeUtf8'

matchApiKey :: T.Text -> ApiKey -> Maybe T.Text
matchApiKey headerValue (ApiKey apiKeyValue) = bool Nothing (Just headerValue) (apiKeyValue == headerValue)


  -- Application = Request -> (Response -> IO ResponseReceived) -> IO ResponseReceivedSource

  -- Middleware = Application -> ApplicationSource