{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE QuasiQuotes         #-}
{-# LANGUAGE ScopedTypeVariables #-}

module SlateApi2Spec where

import Network.Wai.Test

import Test.Tasty.HUnit (Assertion, (@?=))
import SlateApi         (getIndexFile)
import Server           (Except)
import Network.Wai      (Application)

import qualified Web.Scotty.Trans   as ST
import qualified Network.HTTP.Types as H
import qualified Data.ByteString    as B

route :: ST.ScottyT Except IO () -> IO Application
route = ST.scottyAppT id

unit_root :: Assertion
unit_root = do
  app      <- route getIndexFile
  response <- runSession (getRequest "/") app
  let status = simpleStatus response
  status @?= H.status200

getRequest :: B.ByteString -> Session SResponse
getRequest = request . setPath defaultRequest



-- -- defaultRequest :: Request
-- -- defaultRequest = Request
-- --     { requestMethod = H.methodGet
-- --     , httpVersion = H.http10
-- --     , rawPathInfo = B.empty
-- --     , rawQueryString = B.empty
-- --     , requestHeaders = []
-- --     , isSecure = False
-- --     , remoteHost = SockAddrInet 0 0
-- --     , pathInfo = []
-- --     , queryString = []
-- --     , requestBody = return B.empty
-- --     , vault = mempty
-- --     , requestBodyLength = KnownLength 0
-- --     , requestHeaderHost = Nothing
-- --     , requestHeaderRange = Nothing
-- --     , requestHeaderReferer = Nothing
-- --     , requestHeaderUserAgent = Nothing
-- --     }

