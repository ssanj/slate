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
import DB.DBNote              (NoteIdVersion, mkNoteIdVersion, mkNoteId, mkNoteVersion, getNoteText, getInt)

import qualified Web.Scotty.Trans     as ST
import qualified Network.Wai          as W
import qualified Network.HTTP.Types   as H
import qualified Data.ByteString      as B
import qualified Data.ByteString.Lazy as LB
import qualified Data.Aeson           as A
import qualified Data.Text            as T

import Scaffold

route :: ST.ScottyT Except IO () -> IO Application
route = ST.scottyAppT id

--- getIndexFile

unit_root :: Assertion
unit_root = do
  app      <- route getIndexFile
  response <- runSession (getRequest "/") app
  let status = simpleStatus response
  status @?= H.status200


--- getNotesEndpoint

unit_notes :: Assertion
unit_notes = dbWithinTxTest insertSeedDataSearchNotes assertGetNotes


assertGetNotes :: SeededDB -> DBAction ((), CleanUp)
assertGetNotes _ con = do
   app      <- route . getNotesEndpoint $ con
   response <- runSession (getRequest "/notes") app
   let status = simpleStatus response
       body   = simpleBody response
       resultE :: Either String [OutgoingNote] = A.eitherDecode body
       expectedNotes :: [T.Text] =
         [
           "# Some Note\nYolo"
         , "# Another note\nMore and more"
         , "# Random Title\nThis is a blog article about ..."
         , "# Blog Article\nThis is an article about ..."
         , "# Whatever you like\nThis is a BloG article about ..."
         ]
   status @?= H.status200
   either (assertFailure . ("Could not decode result as 'OutgoingNote':" <>)) (assertGetNotesResults expectedNotes) resultE
   pure $ ((), AssertionRun)


assertGetNotesResults ::  [T.Text] -> [OutgoingNote] -> Assertion
assertGetNotesResults expectedNotes actualNotes = do
 (length actualNotes) @?= (length expectedNotes)
 let actualNoteTextList = (_outgoingNoteText) <$> actualNotes

 assertBool
   ("Could not find all expected notes in actual notes.\nExpected notes: " <>
    (show expectedNotes)                                                   <>
    "\nActual: "                                                           <>
    (show actualNoteTextList)
   )
   (all (`elem` expectedNotes) actualNoteTextList)


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
assertSearchNotes _ con = do
   app      <- route . performSearchEndpoint $ con
   response <- runSession (getRequest "/search?q=random") app
   let status = simpleStatus response
       body   = simpleBody response
       resultE :: Either String [OutgoingNote] = A.eitherDecode body
       expectedNotes :: [T.Text] =
         [
           "# Random Title\nThis is a blog article about ..."
         ]
   status @?= H.status200
   either (assertFailure . ("Could not decode result as 'OutgoingNote':" <>)) (assertGetNotesResults expectedNotes) resultE
   pure $ ((), AssertionRun)


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
   let status = simpleStatus response
       body   = simpleBody response
       resultE :: Either String NoteIdVersion = A.eitherDecode body
       expectedNoteIdVersion = mkNoteIdVersion (mkNoteId 1)  (mkNoteVersion 1)
   status @?= H.status201
   either (assertFailure . ("Could not decode result as 'NoteIdVersion':" <>)) (assertCreateNoteResult expectedNoteIdVersion) resultE



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
       incoming = A.encode $
                    A.object [
                      "noteText"    A..= noteMessage
                    , "noteId"      A..= noteId
                    , "noteVersion" A..= noteVersion
                    ]
   response <- runSession (postJSON "/note" incoming) app

   let expectedNote = mkNoteIdVersion (mkNoteId noteId)  (mkNoteVersion noteNewVersion)
   assertResponseStatus response H.status200
   assertResponseBody response expectedNote
   assertNoteInDB noteId noteMessage noteNewVersion con

assertResponseBody :: forall a . (A.FromJSON a, Eq a, Show a) => SResponse -> a -> Assertion
assertResponseBody response expected =
  let body                                   = simpleBody response
      resultE :: Either String a = A.eitherDecode body
  in  either (assertFailure . ("Could not decode result: " <>)) (assertEq' expected) resultE

assertResponseStatus :: SResponse -> H.Status -> Assertion
assertResponseStatus response = assertEq (simpleStatus response)

assertEq :: (Eq a, Show a) => a -> a -> Assertion
assertEq = (@?=)

assertEq' :: (Eq a, Show a) => a -> a -> Assertion
assertEq' = (@=?)

assertNoteInDB :: Int -> T.Text -> Int -> DBAction ()
assertNoteInDB noteId noteMessage noteVersion con = do
   dbNotes <- findDBNote noteId con
   case dbNotes of
    Nothing                           -> assertFailure "Could not find note with id: 1234"
    Just (dbId, dbMessage, dbVersion) -> do
          (getInt dbId)           @?= noteId
          (getNoteText dbMessage) @?= noteMessage
          (getInt dbVersion)      @?= noteVersion


assertCreateNoteResult ::  NoteIdVersion -> NoteIdVersion -> Assertion
assertCreateNoteResult expected actual = actual @?= expected

getRequest :: B.ByteString -> Session SResponse
getRequest = request . setPath defaultRequest

postJSON :: B.ByteString -> LB.ByteString -> Session SResponse
postJSON path json = srequest $ SRequest req json
  where
    req = setPath defaultRequest
            { W.requestMethod = H.methodPost
            , W.requestHeaders = [(H.hContentType, "application/json")]} path