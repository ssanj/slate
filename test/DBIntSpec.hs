{-# LANGUAGE OverloadedStrings #-}

module DBIntSpec where

-- import qualified Hedgehog              as H
-- import qualified Hedgehog.Gen          as Gen
-- import qualified Hedgehog.Range        as Range

import Scaffold

import Test.Tasty.HUnit       ((@?=), Assertion)
import DB.Repository          (fetchNotes, fetchSize, searchNotes, saveNewNote, saveExistingNote, deactivateNote)
import Model                  (DBError(..))
import Data.Foldable          (traverse_)
import Data.Text              (Text)
import Database.SQLite.Simple (execute, query, query_, Only(..))

import qualified DB.DBNote as D


unit_fetchNotes :: Assertion
unit_fetchNotes = dbNoteTest insertSeedDataFetchNotes assert_fetchNotes

unit_fetchNotes_when_no_notes :: Assertion
unit_fetchNotes_when_no_notes = dbNoteTest emptyNotes assert_fetchNotes_when_no_notes

unit_searchNotes :: Assertion
unit_searchNotes = dbNoteTest insertSeedDataSearchNotes assert_searchNotes

unit_searchNotes_no_matches :: Assertion
unit_searchNotes_no_matches = dbNoteTest emptyNotes assert_no_searchNotes

unit_insert_new_note :: Assertion
unit_insert_new_note = dbNoteTest insertSeedDataFetchNotes assert_insert_new_note

unit_insert_existing_note :: Assertion
unit_insert_existing_note = dbNoteTest emptyNotes assert_insert_existing_note

unit_insert_existing_note_not_found :: Assertion
unit_insert_existing_note_not_found = dbNoteTest emptyNotes assert_insert_existing_note_not_found

unit_insert_existing_note_version_mismatch :: Assertion
unit_insert_existing_note_version_mismatch = dbNoteTest emptyNotes assert_insert_existing_note_version_mismatch

unit_insert_existing_note_version_range_error :: Assertion
unit_insert_existing_note_version_range_error = dbNoteTest emptyNotes assert_insert_existing_note_version_range_error

unit_deleted_note_cant_be_updated :: Assertion
unit_deleted_note_cant_be_updated = dbNoteTest emptyNotes assert_deleted_notes_cant_be_updated

unit_deleting_a_note_sets_deleted_flag :: Assertion
unit_deleting_a_note_sets_deleted_flag = dbNoteTest emptyNotes assert_delete_note


-- ASSERTIONS ACTIONS


assert_insert_existing_note_version_range_error :: SeededDB -> DBAction ((), CleanUp)
assert_insert_existing_note_version_range_error _ = \con -> do
  execute con "INSERT INTO SCRIB (ID, MESSAGE, VERSION) VALUES (1000, ?, 2000)" (Only ("Some strange message" :: Text))
  let updatedNoteE = D.createDBNote (D.mkNoteId 1000) "Some message" (D.mkNoteVersion 2000)
  case updatedNoteE of
    Left x -> runAssertionFailure $ "could not create updated note: " <> (show x)
    (Right updatedNote) -> do
      resultE <- saveExistingNote updatedNote con
      case resultE of
        Left dbError ->
          case dbError of
            (InvalidVersion v1) -> runAssertion (v1 @?= 2000)
            otherError            -> runAssertionFailure $ "Expected InvalidVersionRangeError error but got: " <> (show otherError)
        Right found -> runAssertionFailure $ "should not save existing note: " <> (show found)


assert_insert_existing_note_version_mismatch :: SeededDB -> DBAction ((), CleanUp)
assert_insert_existing_note_version_mismatch _ = \con -> do
  execute con "INSERT INTO SCRIB (ID, MESSAGE) VALUES (1000, ?)" (Only ("Some strange message" :: Text))
  let updatedNoteE = D.createDBNote (D.mkNoteId 1000) "Some message" (D.mkNoteVersion 2)
  case updatedNoteE of
    Left x -> runAssertionFailure $ "could not create updated note: " <> (show x)
    (Right updatedNote) -> do
      resultE <- saveExistingNote updatedNote con
      case resultE of
        Left dbError ->
          case dbError of
            (VersionMismatch v1 v2) -> runAssertion $ (v1 @?= 1) >> (v2 @?= 2)
            otherError            -> runAssertionFailure $ "Expected VersionMismatchError error but got: " <> (show otherError)
        Right found -> runAssertionFailure $ "should not save existing note: " <> (show found)


assert_insert_existing_note_not_found :: SeededDB -> DBAction ((), CleanUp)
assert_insert_existing_note_not_found _ = \con -> do
  let updatedNoteE = D.createDBNote (D.mkNoteId 1000) "Some message" (D.mkNoteVersion 1)
  case updatedNoteE of
    Left x -> runAssertionFailure $ "could not create updated note: " <> (show x)
    (Right updatedNote) -> do
      resultE <- saveExistingNote updatedNote con
      case resultE of
        Left dbError ->
          case dbError of
            (ItemNotFound noteId) -> runAssertion $ noteId @?= 1000
            otherError            -> runAssertionFailure $ "Expected ItemNotFound error but got: " <> (show otherError)
        Right found -> runAssertionFailure $ "could not save existing note: " <> (show found)


assert_insert_existing_note :: SeededDB -> DBAction ((), CleanUp)
assert_insert_existing_note _ = \con -> do
  execute con "INSERT INTO SCRIB (ID, MESSAGE) VALUES (1000, ?)" (Only ("Some strange message" :: Text))
  let updatedNoteE = D.createDBNote (D.mkNoteId 1000) "Some message" (D.mkNoteVersion 1)
  case updatedNoteE of
    Left x -> runAssertionFailure $ "could not create updated note: " <> (show x)
    (Right updatedNote) -> do
      resultE <- saveExistingNote updatedNote con
      case resultE of
        Left x2 -> runAssertionFailure $ "could not save existing note: " <> (show x2)
        (Right noteIdVersion) -> do
          let noteId = D.getNoteId noteIdVersion
          noteId                         @?= 1000
          (D.getNoteVersion noteIdVersion) @?= 2
          dbNotes <- query con "SELECT ID, MESSAGE, VERSION FROM SCRIB WHERE ID = ?" (Only noteId) :: IO [D.DBNote]

          case dbNotes of
            [dbNote] ->
              let assertions =
                    [
                      (D.getDBNoteId dbNote)            @?= 1000
                    , (D.getDBNoteVersion dbNote)       @?= 2
                    , (D.getDBNoteText dbNote)          @?= "Some message"
                    ]

              in runAssertion $ sequence_ assertions

            []     -> runAssertionFailure $ "expected to find matching note for id: " <> (show noteId)
            xs     -> runAssertionFailure $ "expected to find one match for note for id: " <> (show noteId) <> "but got: " <> (show xs)


assert_insert_new_note :: SeededDB -> DBAction ((), CleanUp)
assert_insert_new_note _ = \con -> do
  let newNoteE = D.mkNewDBNote "Some very unique text"
  case newNoteE of
    Left x -> runAssertionFailure (show x)
    Right newNote -> do
      noteIdAndVersion <- saveNewNote newNote con
      let nid = D.getNoteId noteIdAndVersion
          nv  = D.getNoteVersion noteIdAndVersion
      notes <- query con "SELECT ID, MESSAGE, VERSION FROM SCRIB WHERE ID = ?" (Only nid)
      case notes of
        [] -> runAssertionFailure $ "expected a matching note with id: " <> (show nid)
        [note] -> do
          nv @?= 1
          runAssertion $ (D.getDBNoteText note) @?= "Some very unique text"
        tooManyNotes -> runAssertionFailure $ "expected one matching note with id: " <> (show nid) <> "but got many: " <> (show tooManyNotes)


assert_fetchNotes :: SeededDB -> DBAction ((), CleanUp)
assert_fetchNotes _ = \con -> do
  notes <- fetchNotes (fetchSize 1) con
  case notes of
    []       -> runAssertionFailure "Expected to find a note"
    (note:_) -> do
      length notes @?= 1
      runAssertion $ assertDBNote note (@?= "# Another note\nMore and more")


assert_fetchNotes_when_no_notes :: SeededDB -> DBAction ((), CleanUp)
assert_fetchNotes_when_no_notes _ = \con -> do
  notes <- fetchNotes (fetchSize 10) con
  case notes of
    []           -> runAssertionSuccess
    tooManyNotes -> runAssertionFailure $ "expected no matching notes, but got many: " <> (show tooManyNotes)


assert_searchNotes :: SeededDB -> DBAction ((), CleanUp)
assert_searchNotes _ = \con -> do
  notes <- searchNotes "blog" con
  case notes of
    []           -> runAssertionFailure "Expected to find 3 matching notes"
    matchedNotes -> do
      let dbNoteNotePairs =
            zip
              (D.getDBNoteText <$> matchedNotes)
              [
                -- we ensure the hits are ordered newest updates to oldest
                "# Whatever you like\nThis is a BloG article about ..." -- match irrespective of case
              , "# Random Title\nThis is a blog article about ..."      -- case match
              , "# Blog Article\nThis is an article about ..."          -- match on title
              ] :: [(Text, Text)]
      runAssertion $ (traverse_ (\pairs -> (fst pairs) @?= (snd pairs)) dbNoteNotePairs)


assert_no_searchNotes :: SeededDB -> DBAction ((), CleanUp)
assert_no_searchNotes _ = \con -> do
  notes <- searchNotes "blog" con
  case notes of
    []           -> runAssertionSuccess
    matchedNotes -> runAssertionFailure $ "Expected no search matches but got: " <> (show matchedNotes)


assert_deleted_notes_cant_be_updated :: SeededDB -> DBAction ((), CleanUp)
assert_deleted_notes_cant_be_updated _ = \con -> do
  execute con "INSERT INTO SCRIB (ID, MESSAGE, DELETED) VALUES (1000, ?, 1)" (Only ("Some strange message" :: Text))
  let updatedNoteE = D.createDBNote (D.mkNoteId 1000) "Some new message" (D.mkNoteVersion 1)
  case updatedNoteE of
    Left x -> runAssertionFailure $ "could not create updated note: " <> (show x)
    (Right updatedNote) -> do
      resultE <- saveExistingNote updatedNote con
      case resultE of
        Left dbError ->
          case dbError of
            (UpdatingDeletedNote _) -> runAssertionSuccess
            otherError              -> runAssertionFailure $ "Expected UpdatingDeletedNote error but got: " <> (show otherError)
        Right found -> runAssertionFailure $ "should not save a deleted note: " <> (show found)


assert_delete_note :: SeededDB -> DBAction ((), CleanUp)
assert_delete_note _ = \con -> do
  execute con "INSERT INTO SCRIB (ID, MESSAGE) VALUES (1000, ?)" (Only ("Some strange message1" :: Text))
  execute con "INSERT INTO SCRIB (ID, MESSAGE) VALUES (1001, ?)" (Only ("Some strange message2" :: Text))
  execute con "INSERT INTO SCRIB (ID, MESSAGE) VALUES (1002, ?)" (Only ("Some strange message3" :: Text))
  let noteId = D.mkNoteId 1001
  resultsBefore <- query_ con "SELECT ID, DELETED FROM SCRIB ORDER BY ID" :: IO [(Int, Bool)]
  case resultsBefore of
    [t0,t1,t2] -> do
      t0 @?= (1000, False)
      t1 @?= (1001, False)
      t2 @?= (1002, False)
      deactivateNote noteId con
      resultsAfter <- query_ con "SELECT ID, DELETED FROM SCRIB ORDER BY ID" :: IO [(Int, Bool)]
      case resultsAfter of
        [t0', t1', t2'] -> do
          t0' @?= (1000, False)
          t1' @?= (1001, True)
          t2' @?= (1002, False)
          runAssertionSuccess
        others              -> runAssertionFailure $ "Expected three results with ids: 1000,1001,1002 but got:" <> (show others)
    others              -> runAssertionFailure $ "Expected three results with ids: 1000,1001,1002 but got: " <> (show others)


-- DATABASE SEED DATA


emptyNotes :: InitialisedDB -> DBAction ((), SeededDB)
emptyNotes _ = \_ ->  pure ((), SeededDB)


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


insertSeedDataFetchNotes :: InitialisedDB -> DBAction ((), SeededDB)
insertSeedDataFetchNotes _ = \con -> do
  traverse_ (\n -> insertMessageNumbered n con) [1..20]
  traverse_
    (\msg -> insertMessage msg con)
    [
      ("# Some Note\nYolo", "2020-06-01T15:36:56.200", False)
    , ("# Another note\nMore and more", "2020-06-09T15:36:56.200", False)
    , ("# This note is deleted\nSome random note", "2021-02-16T09:30:32.200", True)
    , ("# Blog Article\nThis is a blog article about..", "2020-06-02T15:36:56.200", False)
    , ("# Another deleted note\nThis is an old blog article about..", "2020-06-02T15:36:56.200", True)
    ]
  pure ((), SeededDB)

