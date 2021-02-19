{-# LANGUAGE OverloadedStrings, QuasiQuotes #-}
module SlateApiSpec where

import           Test.Hspec
import           Test.Hspec.Wai
-- import           Test.Hspec.Wai.JSON

-- import qualified Web.Scotty as S
-- import           Data.Aeson (Value(..), object, (.=))

import qualified  Web.Scotty.Trans as ST

import            SlateApi (getIndexFile)
import            Server   (Except)
import            Network.Wai (Application)



-- app :: IO Application
-- app = S.scottyApp $ do
--   S.get "/" $ do
--     S.text "hello"

--   S.get "/some-json" $ do
--     S.json $ object ["foo" .= Number 23, "bar" .= Number 42]

route :: ST.ScottyT Except IO () -> IO Application
route = ST.scottyAppT id

spec_root :: Spec
spec_root =
    with (route getIndexFile) $ do
    describe "GET /" $ do
      it "responds with 200" $ do
        get "/" `shouldRespondWith` 200

  --   it "responds with 'hello'" $ do
  --     get "/" `shouldRespondWith` "hello"

  --   it "responds with 200 / 'hello'" $ do
  --     get "/" `shouldRespondWith` "hello" {matchStatus = 200}

  --   it "has 'Content-Type: text/plain; charset=utf-8'" $ do
  --     get "/" `shouldRespondWith` 200 {matchHeaders = ["Content-Type" <:> "text/plain; charset=utf-8"]}

  -- describe "GET /some-json" $ do
  --   it "responds with some JSON" $ do
  --     get "/some-json" `shouldRespondWith` [json|{foo: 23, bar: 42}|]