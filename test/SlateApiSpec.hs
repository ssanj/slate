{-# LANGUAGE OverloadedStrings, QuasiQuotes #-}
{-# LANGUAGE QuasiQuotes                    #-}
{-# LANGUAGE ScopedTypeVariables            #-}

module SlateApiSpec where

-- import           Test.Hspec
-- import           Test.Hspec.Wai

-- import qualified  Web.Scotty.Trans       as ST
-- import qualified  Data.ByteString.Lazy   as LB
-- import qualified  Data.Text              as T
-- import qualified  Data.Text.Encoding     as TE
-- import qualified  Database.SQLite.Simple as SQL
-- import            SlateApi (getIndexFile)
-- import            Server   (Except)
-- import            Network.Wai (Application)
-- import            Control.Monad (void)

-- import Scaffold

-- spec :: Spec
-- spec = withState (return 42) :: IO Int $
--   describe "This magical number" $
--     it "is bigger than 40" $ \n ->
--       n `shouldSatisfy` (>40)

-- route :: ST.ScottyT Except IO () -> IO Application
-- route = ST.scottyAppT id

-- spec_root :: Spec
-- spec_root =
--     with (route getIndexFile) $ do
    -- describe "GET /" $ do
    --   it "responds with 200" $ do
    --     get "/" `shouldRespondWith` 200 { matchBody =  bodyContaining "<title>Scrib - Home</title>" }

    -- withState (xyz) $ do
    --   describe "GET /" $ do
    --     it "responds with 200" $ \(_::SQL.Connection) -> do
    --       get "/" `shouldRespondWith` 200 { matchBody =  bodyContaining "<title>Scrib - Home</title>" }


    -- it "responds with 'hello'" $ do
    --   get "/notes" `shouldRespondWith` 401

  --   it "responds with 200 / 'hello'" $ do
  --     get "/" `shouldRespondWith` "hello" {matchStatus = 200}

  --   it "has 'Content-Type: text/plain; charset=utf-8'" $ do
  --     get "/" `shouldRespondWith` 200 {matchHeaders = ["Content-Type" <:> "text/plain; charset=utf-8"]}

  -- describe "GET /some-json" $ do
  --   it "responds with some JSON" $ do
  --     get "/some-json" `shouldRespondWith` [json|{foo: 23, bar: 42}|]




-- bodyContaining :: T.Text -> MatchBody
-- bodyContaining expectedText =
--   MatchBody (\_ -> containsMatchingText)
--     where
--       containsMatchingText :: LB.ByteString -> Maybe String
--       containsMatchingText actual =
--         let actualText = TE.decodeUtf8 . LB.toStrict $ actual
--         in
--           if expectedText `T.isInfixOf` actualText then Nothing
--           else Just $ "\nexpected: " <> (show expectedText) <> "\nbut got: " <> (show actualText)