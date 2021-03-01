{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE QuasiQuotes         #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}

module SlateApi2Spec where

import Network.Wai.Test

import Test.Tasty.HUnit       (Assertion, (@?=))
import SlateApi               (getIndexFile, getNotes2)
import Server                 (Except)
import Network.Wai            (Application)
import Data.Foldable          (traverse_)

import qualified Web.Scotty.Trans   as ST
import qualified Network.HTTP.Types as H
import qualified Data.ByteString    as B


import Scaffold

route :: ST.ScottyT Except IO () -> IO Application
route = ST.scottyAppT id

unit_root :: Assertion
unit_root = do
  app      <- route getIndexFile
  response <- runSession (getRequest "/") app
  let status = simpleStatus response
  status @?= H.status200

unit_notes :: Assertion
unit_notes = do
  let dbWrap ::  (SeededDB -> DBAction ((), CleanUp)) -> IO () = dbWithinTxTest insertSeedDataSearchNotes
  dbWrap appAssertion --(\_ -> fmap (,AssertionRun) . route . getNotes2)
    where
        appAssertion :: SeededDB -> DBAction ((), CleanUp)
        appAssertion _ con = do
           app      <- route . getNotes2 $ con
           response <- runSession (getRequest "/notes") app
           let status = simpleStatus response
           status @?= H.status200
           pure $ ((), AssertionRun)

getRequest :: B.ByteString -> Session SResponse
getRequest = request . setPath defaultRequest


insertSeedDataSearchNotes :: InitialisedDB -> DBAction ((), SeededDB)
insertSeedDataSearchNotes _ = \con -> do
  traverse_
    (\msg -> insertMessage msg con)
    [
      ("# Some Note\nYolo", "2020-06-01T15:36:56.200", False)
    , ("# Another note\nMore and more", "2020-06-09T15:36:56.200", False)
    , ("# Random Title\nThis is a blog article about ...", "2020-06-02T15:36:56.200", False)
    , ("# Blog Article\nThis is an article about ...", "2020-06-01T15:36:56.200", False)
    , ("# Deleted Title\nThis is a deleted blog article about ...", "2020-06-02T15:36:56.200", True)
    , ("# Whatever you like\nThis is a BloG article about ...", "2020-09-02T15:36:56.200", False)
    ]
  putStrLn "after seed"
  pure ((), SeededDB)

-- dbAppTest :: (InitialisedDB -> DBAction ((), SeededDB)) -> (SeededDB -> DBAction (a, CleanUp)) -> IO a


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

