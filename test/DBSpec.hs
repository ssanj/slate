{-# LANGUAGE OverloadedStrings #-}

module DBSpec where

-- import qualified Hedgehog              as H
-- import qualified Hedgehog.Gen          as Gen
-- import qualified Hedgehog.Range        as Range

import Test.Tasty.HUnit ((@?=), Assertion, assertFailure)
import DB               (fetchNotes, fetchSize)
import Data.Foldable    (traverse_)
import Scaffold

-- hprop_equality :: H.Property
-- hprop_equality =
--   H.property $ do
--     randomString <- H.forAll $ Gen.text (Range.linear 0 100) Gen.alpha
--     randomString H.=== randomString

unit_fetchNotes :: Assertion
unit_fetchNotes = dbTest testDatabaseName $ DBTest createSchema insertSeedData assert_fetchNotes deleteSchema

assert_fetchNotes :: SeededDB -> DBAction ((), CleanUp)
assert_fetchNotes _ = \con -> do
  notes <- fetchNotes (fetchSize 1) con
  case notes of
    []       -> (\_ -> ((), AssertionRun)) <$> assertFailure "Expected to find a note"
    (note:_) -> (\_ -> ((), AssertionRun)) <$> assertDBNote note (@?= "# Another note\nMore and more")

insertSeedData :: InitialisedDB -> DBAction ((), SeededDB)
insertSeedData _ = \con -> do
  traverse_ (\n -> insertMessageNumbered n con) [1..20]
  traverse_
    (\msg -> insertMessage msg con)
    [
      ("# Some Note\nYolo", "2020-06-01T15:36:56.200")
    , ("# Another note\nMore and more", "2020-06-09T15:36:56.200")
    , ("# Blog Article\nThis is a blog article about..", "2020-06-02T15:36:56.200")
    ]
  pure ((), SeededDB)