{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE QuasiQuotes         #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}

module SlateApiSpec where

import Network.Wai.Test

import Test.Tasty.HUnit       (Assertion, assertFailure, assertBool, (@?=), (@=?))
import SlateApi               (getIndexFile, getNotesEndpoint, performSearchEndpoint, createNoteEndpoint, deleteNoteEndpoint)
import Server                 (Except)
import Network.Wai            (Application)
import Data.Foldable          (traverse_)
import Model                  (OutgoingNote(..))
import DB.DBNote              (mkNoteIdVersion, mkNoteId, mkNoteVersion, getNoteText, getInt, getBool)
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
insertSeedDataSearchNotes _ = \con -> runSeeding $ do
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




-- createNoteEndpoint

unit_create_note :: Assertion
unit_create_note = dbWithinTxTest noTestData assertCreateNote


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


unit_update_note :: Assertion
unit_update_note = dbWithinTxTest singleNote assertUpdateNote


singleNote :: InitialisedDB -> DBAction ((), SeededDB)
singleNote _ = \con -> runSeeding $ do
  insertSpecificMessage 1234 "Some message" con


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


-- deleteNote endpoint

unit_delete_note_no_matching_notes :: Assertion
unit_delete_note_no_matching_notes = dbWithinTxTest noTestData assertDeleteNoteUnmatchedNote


unit_delete_note_matching_notes :: Assertion
unit_delete_note_matching_notes = dbWithinTxTest undeletedNotes assertDeletedOnlyMatchedNote


unit_delete_note_matching_notes_already_deleted :: Assertion
unit_delete_note_matching_notes_already_deleted =
  dbWithinTxTest
    deletedNotesWithOneDeleted
    assertDidNotDeleteDeletedNote


undeletedNotes :: InitialisedDB -> DBAction ((), SeededDB)
undeletedNotes _ = \con -> runSeeding $ do
  traverse_
    (\(index, msg) -> insertSpecificMessage index msg con)
    [
      (1000, "# Note 1000")
    , (1001, "# Note 1001")
    , (1002, "# Note 1002")
    ]


deletedNotesWithOneDeleted :: InitialisedDB -> DBAction ((), SeededDB)
deletedNotesWithOneDeleted _ = \con -> runSeeding $ do
  traverse_
    (\(index, msg, d) -> insertSpecificMessageWithDeletion index msg d con)
    [
      (1000, "# Note 1000", False)
    , (1001, "# Note 1001", True)
    , (1002, "# Note 1002", False)
    ]


assertDeleteNoteUnmatchedNote :: SeededDB -> DBAction ((), CleanUp)
assertDeleteNoteUnmatchedNote _ = \con -> runAssertion $ do
  app      <- route . deleteNoteEndpoint $ con
  response <- runSession (deleteRequest "/note/1000") app

  traverse_
    (response &)
    [
      assertResponseStatus H.status400
    ]


assertDeletedOnlyMatchedNote :: SeededDB -> DBAction ((), CleanUp)
assertDeletedOnlyMatchedNote _ = \con -> runAssertion $ do
  app      <- route . deleteNoteEndpoint $ con
  response <- runSession (deleteRequest "/note/1001") app

  traverse_
    (response &)
    [
      assertResponseStatus H.status204
    ]


  assertNoteInDBIsDeleted 1000 False con
  assertNoteInDBIsDeleted 1001 True  con
  assertNoteInDBIsDeleted 1002 False con


assertDidNotDeleteDeletedNote :: SeededDB -> DBAction ((), CleanUp)
assertDidNotDeleteDeletedNote _ = \con -> runAssertion $ do
  app      <- route . deleteNoteEndpoint $ con
  response <- runSession (deleteRequest "/note/1001") app

  traverse_
    (response &)
    [
      assertResponseStatus H.status400
    ]


  assertNoteInDBIsDeleted 1000 False con
  assertNoteInDBIsDeleted 1001 True  con
  assertNoteInDBIsDeleted 1002 False con


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
    Just (dbId, dbMessage, dbVersion, _) -> do
          (getInt dbId)           @?= noteId
          (getNoteText dbMessage) @?= noteMessage
          (getInt dbVersion)      @?= noteVersion

assertNoteInDBIsDeleted :: Int -> Bool -> DBAction ()
assertNoteInDBIsDeleted noteId deletedFlag con = do
   dbNotes <- findDBNote noteId con
   case dbNotes of
    Nothing                           -> assertFailure $ "Could not find note with id: " <> (show noteId)
    Just (dbId, _, _, dbDeleted) -> do
          (getInt dbId)           @?= noteId
          (getBool dbDeleted)     @?= deletedFlag


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

deleteRequest :: B.ByteString -> Session SResponse
deleteRequest = request . setPath defaultRequest { W.requestMethod = H.methodDelete }


postJSON :: B.ByteString -> LB.ByteString -> Session SResponse
postJSON path json = srequest $ SRequest req json
  where
    req = setPath defaultRequest
            { W.requestMethod = H.methodPost
            , W.requestHeaders = [(H.hContentType, "application/json")]} path


route :: ST.ScottyT Except IO () -> IO Application
route = ST.scottyAppT id
