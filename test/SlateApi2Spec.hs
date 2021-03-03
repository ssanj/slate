{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE QuasiQuotes         #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}

module SlateApi2Spec where

import Network.Wai.Test

import Test.Tasty.HUnit       (Assertion, assertFailure, assertBool, (@?=), (@=?))
import SlateApi               (getIndexFile, getNotesEndpoint, performSearchEndpoint, createNoteEndpoint)
import Server                 (Except)
import Network.Wai            (Application)
import Data.Foldable          (traverse_)
import Model                  (OutgoingNote(..))
import DB.DBNote              (mkNoteIdVersion, mkNoteId, mkNoteVersion, getNoteText, getInt)
import Data.Function          ((&))

import qualified Web.Scotty.Trans     as ST
import qualified Network.Wai          as W
import qualified Network.HTTP.Types   as H
import qualified Data.ByteString      as B
import qualified Data.ByteString.Lazy as LB
import qualified Data.Aeson           as A
import qualified Data.Text            as T

import Scaffold


--- getIndexFile

unit_root :: Assertion
unit_root = do
  app      <- route getIndexFile
  response <- runSession (getRequest "/") app

  assertResponseStatus H.status200 response


--- getNotesEndpoint

unit_notes :: Assertion
unit_notes = dbWithinTxTest insertSeedDataSearchNotes assertGetNotes


assertGetNotes :: SeededDB -> DBAction ((), CleanUp)
assertGetNotes _ con = runAssertion $ do
   app      <- route . getNotesEndpoint $ con
   response <- runSession (getRequest "/notes") app
   let expectedNotes :: [T.Text] =
         [
           "# Some Note\nYolo"
         , "# Another note\nMore and more"
         , "# Random Title\nThis is a blog article about ..."
         , "# Blog Article\nThis is an article about ..."
         , "# Whatever you like\nThis is a BloG article about ..."
         ]

   traverse_
    (response &)
    [
      assertResponseStatus H.status200
    , assertResponseBodyCollection expectedNotes _outgoingNoteText
    ]


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
  pure ((), SeededDB)


--- performSearchEndpoint

unit_perform_search :: Assertion
unit_perform_search = dbWithinTxTest insertSeedDataSearchNotes assertSearchNotes


assertSearchNotes :: SeededDB -> DBAction ((), CleanUp)
assertSearchNotes _ con = runAssertion $ do
   app      <- route . performSearchEndpoint $ con
   response <- runSession (getRequest "/search?q=random") app

   let expectedNotes :: [T.Text] =
         [
           "# Random Title\nThis is a blog article about ..."
         ]

   traverse_
    (response & )
    [
      assertResponseStatus H.status200
    , assertResponseBodyCollection expectedNotes _outgoingNoteText
    ]


unit_create_note :: Assertion
unit_create_note = dbWithinTxTest noTestData assertCreateNote


unit_update_note :: Assertion
unit_update_note = dbWithinTxTest singleNote assertUpdateNote


-- createNoteEndpoint

assertCreateNote :: SeededDB -> DBAction ((), CleanUp)
assertCreateNote _ con = runAssertion $ do
   app      <- route . createNoteEndpoint $ con
   let incoming = A.encode $ A.object [ "noteText" A..= ("Sample text" :: T.Text)]
   response <- runSession (postJSON "/note" incoming) app
   let expectedNoteIdVersion = mkNoteIdVersion (mkNoteId 1)  (mkNoteVersion 1)

   traverse_
    (response &)
    [
      assertResponseStatus H.status201
    , assertResponseBody expectedNoteIdVersion
    ]


singleNote :: InitialisedDB -> DBAction ((), SeededDB)
singleNote _ = \con -> do
  insertSpecificMessage 1234 "Some message" con
  pure ((), SeededDB)


assertUpdateNote :: SeededDB -> DBAction ((), CleanUp)
assertUpdateNote _ con = runAssertion $ do
   app      <- route . createNoteEndpoint $ con
   let noteId         = 1234 :: Int
       noteMessage    = "Some other message" :: T.Text
       noteVersion    = 1 :: Int
       noteNewVersion = 2 :: Int
       incoming       = A.encode $
                          A.object [
                            "noteText"    A..= noteMessage
                          , "noteId"      A..= noteId
                          , "noteVersion" A..= noteVersion
                          ]
   response <- runSession (postJSON "/note" incoming) app

   let expectedNote = mkNoteIdVersion (mkNoteId noteId)  (mkNoteVersion noteNewVersion)

   traverse_
    (response &)
    [
      assertResponseStatus H.status200
    , assertResponseBody expectedNote
    ]

   assertNoteInDB noteId noteMessage noteNewVersion con


-- Helper functions


assertResponseBody :: forall a . (A.FromJSON a, Eq a, Show a) => a -> SResponse -> Assertion
assertResponseBody expected response =
  let body                       = simpleBody response
      resultE :: Either String a = A.eitherDecode body
  in  either (assertFailure . ("Could not decode result: " <>)) (assertEq' expected) resultE


assertResponseStatus :: H.Status -> SResponse -> Assertion
assertResponseStatus status response = assertEq (simpleStatus response) status


assertEq :: (Eq a, Show a) => a -> a -> Assertion
assertEq = (@?=)


assertEq' :: (Eq a, Show a) => a -> a -> Assertion
assertEq' = (@=?)


assertNoteInDB :: Int -> T.Text -> Int -> DBAction ()
assertNoteInDB noteId noteMessage noteVersion con = do
   dbNotes <- findDBNote noteId con
   case dbNotes of
    Nothing                           -> assertFailure $ "Could not find note with id: " <> (show noteId)
    Just (dbId, dbMessage, dbVersion) -> do
          (getInt dbId)           @?= noteId
          (getNoteText dbMessage) @?= noteMessage
          (getInt dbVersion)      @?= noteVersion


assertResponseBodyCollection :: forall a b . (A.FromJSON a, Eq b, Show b) => [b] -> (a -> b) -> SResponse -> Assertion
assertResponseBodyCollection expected convertResponse response =
  let body                       = simpleBody response
      resultE :: Either String [a] = A.eitherDecode body
  in  either (assertFailure . ("Could not decode result: " <>)) (assertCollectionResults expected . fmap convertResponse) resultE


assertCollectionResults :: (Eq a, Show a) => [a] -> [a] -> Assertion
assertCollectionResults expectedItems actualItems = do
 (length actualItems) @?= (length expectedItems)

 assertBool
   ("Could not find all expected items in actual items.\nExpected: " <>
    (show expectedItems)                                             <>
    "\nActual: "                                                     <>
    (show actualItems)
   )
   (all (`elem` expectedItems) actualItems)


getRequest :: B.ByteString -> Session SResponse
getRequest = request . setPath defaultRequest


postJSON :: B.ByteString -> LB.ByteString -> Session SResponse
postJSON path json = srequest $ SRequest req json
  where
    req = setPath defaultRequest
            { W.requestMethod = H.methodPost
            , W.requestHeaders = [(H.hContentType, "application/json")]} path


route :: ST.ScottyT Except IO () -> IO Application
route = ST.scottyAppT id
