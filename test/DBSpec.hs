{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module DBSpec where

import Model.DBNote
import Test.Tasty.HUnit       ((@?=), Assertion, assertFailure)

unit_determineUpdate_note_not_found :: Assertion
unit_determineUpdate_note_not_found =
  let dbNoteE = createDBNote (mkNoteId 1000) "blee" (mkNoteVersion 1)
  in case dbNoteE of
      Left x       -> assertFailure (show x)
      Right dbNote ->
        let result = determineUpdate dbNote Nothing undefined
        in
          case result of
            NoMatchingNoteFound noteId -> noteId @?= 1000
            other -> assertFailure $ "expected NoMatchingNoteFound but got: " <> (show other)


unit_determineUpdate_doUpdate :: Assertion
unit_determineUpdate_doUpdate =
  let dbNoteE = createDBNote (mkNoteId 1000) "blee" (mkNoteVersion 1)
  in case dbNoteE of
      Left x       -> assertFailure (show x)
      Right dbNote ->
        let result = determineUpdate dbNote (Just $ mkNoteVersionAndDeletetionFromDB 1 False) (VersionRange 1 3)
        in
          case result of
            (DoUpdate noteId  noteMessage dbVersion updatedVersion) -> do
              noteId                         @?= (mkNoteId 1000)
              (getNoteText noteMessage)      @?= "blee"
              (getAnyVersion dbVersion)      @?= 1
              (getAnyVersion updatedVersion) @?= 2

            other -> assertFailure $ "expected DoUpdate but got: " <> (show other)

unit_determineUpdate_versionMismatch :: Assertion
unit_determineUpdate_versionMismatch =
  let dbNoteE = createDBNote (mkNoteId 1000) "blee" (mkNoteVersion 2)
  in case dbNoteE of
      Left x       -> assertFailure (show x)
      Right dbNote ->
        let result = determineUpdate dbNote (Just $ mkNoteVersionAndDeletetionFromDB 1 False) (VersionRange 1 5)
        in
          case result of
            (VersionMismatchError v1 v2) -> do
              (getAnyVersion v1) @?= 1
              (getAnyVersion v2) @?= 2

            other -> assertFailure $ "expected VersionMismatchError but got: " <> (show other)

unit_determineUpdate_invalidVersionRange_diff_note_version :: Assertion
unit_determineUpdate_invalidVersionRange_diff_note_version =
  let dbNoteE = createDBNote (mkNoteId 1000) "blee" (mkNoteVersion 10)
  in case dbNoteE of
      Left x       -> assertFailure (show x)
      Right dbNote ->
        let result = determineUpdate dbNote (Just $ mkNoteVersionAndDeletetionFromDB 1 False) (VersionRange 1 5)
        in
          case result of
            (InvalidVersionRangeError v1) -> v1 @?= 10
            other -> assertFailure $ "expected InvalidVersionRangeError but got: " <> (show other)

unit_determineUpdate_invalidVersionRange_same_note_version :: Assertion
unit_determineUpdate_invalidVersionRange_same_note_version =
  let dbNoteE = createDBNote (mkNoteId 1000) "blee" (mkNoteVersion 10)
  in case dbNoteE of
      Left x       -> assertFailure (show x)
      Right dbNote ->
        let result = determineUpdate dbNote (Just $ mkNoteVersionAndDeletetionFromDB 10 False) (VersionRange 1 5)
        in
          case result of
            (InvalidVersionRangeError v1) -> v1 @?= 10
            other -> assertFailure $ "expected InvalidVersionRangeError but got: " <> (show other)

-- Add a test for when delete is true