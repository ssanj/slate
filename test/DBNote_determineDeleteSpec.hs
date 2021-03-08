{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module DBNote_determineDeleteSpec where

import DB.DBNote
import Test.Tasty.HUnit       ((@?=), Assertion, assertFailure)


unit_determineUpdate_doDelete :: Assertion
unit_determineUpdate_doDelete =
  let nId         = 1000 :: Int
      noteId      = mkNoteId nId
      noteDeleted = False
      dbNote      = mkNoteIdAndDeletetionFromDB nId noteDeleted
      result      = determineDelete noteId (Just dbNote)
  in case result of
      DoDelete noteId' -> noteId' @?= noteId
      other            -> assertFailure $ "expected DoDelete but got: " <> (show other)


unit_determineUpdate_notFound :: Assertion
unit_determineUpdate_notFound =
  let nId         = 1000 :: Int
      noteId      = mkNoteId nId
      result      = determineDelete noteId Nothing
  in case result of
      NoteNotFound noteId' -> noteId' @?= noteId
      other                -> assertFailure $ "expected NotFound but got: " <> (show other)


unit_determineUpdate_alreadyDeleted :: Assertion
unit_determineUpdate_alreadyDeleted =
  let nId          = 1000 :: Int
      noteId       = mkNoteId nId
      existingNote = Just $ mkNoteIdAndDeletetionFromDB 1000 True
      result       = determineDelete noteId existingNote
  in case result of
      NoteAlreadyDeleted noteId' -> noteId' @?= noteId
      other                -> assertFailure $ "expected NoteAlreadyDeleted but got: " <> (show other)


-- unit_determineUpdate_doUpdate :: Assertion
-- unit_determineUpdate_doUpdate =
--   let dbNoteE = createDBNote (mkNoteId 1000) "blee" (mkNoteVersion 1)
--   in case dbNoteE of
--       Left x       -> assertFailure (show x)
--       Right dbNote ->
--         let result = determineUpdate dbNote (Just $ mkNoteVersionAndDeletetionFromDB 1 False) (VersionRange 1 3)
--         in
--           case result of
--             (DoUpdate noteId  noteMessage dbVersion updatedVersion) -> do
--               noteId                         @?= (mkNoteId 1000)
--               (getNoteText noteMessage)      @?= "blee"
--               (getAnyVersion dbVersion)      @?= 1
--               (getAnyVersion updatedVersion) @?= 2

--             other -> assertFailure $ "expected DoUpdate but got: " <> (show other)

-- unit_determineUpdate_versionMismatch :: Assertion
-- unit_determineUpdate_versionMismatch =
--   let dbNoteE = createDBNote (mkNoteId 1000) "blee" (mkNoteVersion 2)
--   in case dbNoteE of
--       Left x       -> assertFailure (show x)
--       Right dbNote ->
--         let result = determineUpdate dbNote (Just $ mkNoteVersionAndDeletetionFromDB 1 False) (VersionRange 1 5)
--         in
--           case result of
--             (VersionMismatchError v1 v2) -> do
--               (getAnyVersion v1) @?= 1
--               (getAnyVersion v2) @?= 2

--             other -> assertFailure $ "expected VersionMismatchError but got: " <> (show other)

-- unit_determineUpdate_invalidVersionRange_diff_note_version :: Assertion
-- unit_determineUpdate_invalidVersionRange_diff_note_version =
--   let dbNoteE = createDBNote (mkNoteId 1000) "blee" (mkNoteVersion 10)
--   in case dbNoteE of
--       Left x       -> assertFailure (show x)
--       Right dbNote ->
--         let result = determineUpdate dbNote (Just $ mkNoteVersionAndDeletetionFromDB 1 False) (VersionRange 1 5)
--         in
--           case result of
--             (InvalidVersionRangeError v1) -> v1 @?= 10
--             other -> assertFailure $ "expected InvalidVersionRangeError but got: " <> (show other)

-- unit_determineUpdate_invalidVersionRange_same_note_version :: Assertion
-- unit_determineUpdate_invalidVersionRange_same_note_version =
--   let dbNoteE = createDBNote (mkNoteId 1000) "blee" (mkNoteVersion 10)
--   in case dbNoteE of
--       Left x       -> assertFailure (show x)
--       Right dbNote ->
--         let result = determineUpdate dbNote (Just $ mkNoteVersionAndDeletetionFromDB 10 False) (VersionRange 1 5)
--         in
--           case result of
--             (InvalidVersionRangeError v1) -> v1 @?= 10
--             other -> assertFailure $ "expected InvalidVersionRangeError but got: " <> (show other)

-- unit_determineUpdate_cant_update_deleted_note :: Assertion
-- unit_determineUpdate_cant_update_deleted_note =
--   let dbNoteE = createDBNote (mkNoteId 1000) "blee" (mkNoteVersion 1)
--   in case dbNoteE of
--       Left x       -> assertFailure (show x)
--       Right dbNote ->
--         let result = determineUpdate dbNote (Just $ mkNoteVersionAndDeletetionFromDB 1 True) (VersionRange 1 5)
--         in
--           case result of
--             CantUpdateDeletedNote -> pure ()
--             other -> assertFailure $ "expected CantUpdateDeletedNote but got: " <> (show other)

