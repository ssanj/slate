{-# LANGUAGE OverloadedStrings #-}

module DBSpec where

-- import qualified Hedgehog              as H
-- import qualified Hedgehog.Gen          as Gen
-- import qualified Hedgehog.Range        as Range

import Test.Tasty.HUnit ((@?=), Assertion)
import DB               (fetchNotes, fetchSize, searchNotes)
import Model.DBNote     (getDBNoteText)
import Data.Foldable    (traverse_)
import Data.Text        (Text)

import Scaffold

-- hprop_equality :: H.Property
-- hprop_equality =
--   H.property $ do
--     randomString <- H.forAll $ Gen.text (Range.linear 0 100) Gen.alpha
--     randomString H.=== randomString

unit_fetchNotes :: Assertion
unit_fetchNotes = dbTest testDatabaseName $ DBTest createSchema insertSeedDataFetchNotes assert_fetchNotes deleteSchema

unit_searchNotes :: Assertion
unit_searchNotes = dbTest testDatabaseName $ DBTest createSchema insertSeedDataSearchNotes assert_searchNotes deleteSchema

unit_searchNotes_no_matches :: Assertion
unit_searchNotes_no_matches = dbTest testDatabaseName $ DBTest createSchema emptyNotes assert_no_searchNotes deleteSchema


-- ASSERTIONS ACTIONS


assert_fetchNotes :: SeededDB -> DBAction ((), CleanUp)
assert_fetchNotes _ = \con -> do
  notes <- fetchNotes (fetchSize 1) con
  case notes of
    []       -> runAssertionFailure "Expected to find a note"
    (note:_) -> do
      length notes @?= 1
      runAssertion $ assertDBNote note (@?= "# Another note\nMore and more")

assert_searchNotes :: SeededDB -> DBAction ((), CleanUp)
assert_searchNotes _ = \con -> do
  notes <- searchNotes "blog" con
  case notes of
    []           -> runAssertionFailure "Expected to find 3 matching notes"
    matchedNotes -> do
      let dbNoteNotePairs =
            zip
              (getDBNoteText <$> matchedNotes)
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


-- DATABASE SEED DATA


emptyNotes :: InitialisedDB -> DBAction ((), SeededDB)
emptyNotes _ = \_ ->  pure ((), SeededDB)

insertSeedDataSearchNotes :: InitialisedDB -> DBAction ((), SeededDB)
insertSeedDataSearchNotes _ = \con -> do
  traverse_
    (\msg -> insertMessage msg con)
    [
      ("# Some Note\nYolo", "2020-06-01T15:36:56.200")
    , ("# Another note\nMore and more", "2020-06-09T15:36:56.200")
    , ("# Random Title\nThis is a blog article about ...", "2020-06-02T15:36:56.200")
    , ("# Blog Article\nThis is an article about ...", "2020-06-01T15:36:56.200")
    , ("# Whatever you like\nThis is a BloG article about ...", "2020-09-02T15:36:56.200")
    ]
  pure ((), SeededDB)

insertSeedDataFetchNotes :: InitialisedDB -> DBAction ((), SeededDB)
insertSeedDataFetchNotes _ = \con -> do
  traverse_ (\n -> insertMessageNumbered n con) [1..20]
  traverse_
    (\msg -> insertMessage msg con)
    [
      ("# Some Note\nYolo", "2020-06-01T15:36:56.200")
    , ("# Another note\nMore and more", "2020-06-09T15:36:56.200")
    , ("# Blog Article\nThis is a blog article about..", "2020-06-02T15:36:56.200")
    ]
  pure ((), SeededDB)