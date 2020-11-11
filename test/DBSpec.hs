{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module DBSpec where

import qualified Hedgehog              as H
import qualified Hedgehog.Gen          as Gen
import qualified Hedgehog.Range        as Range
import qualified Model.DBNote          as D

import Model.DBNote
import Test.Tasty.HUnit       ((@?=), Assertion, assertFailure)

hprop_versionRange :: H.Property
hprop_versionRange =
  H.property $ do
    minR    <- H.forAll $ Gen.int (Range.linear 0 99)
    -- inc maxR so that we don't have an instance where minR == maxR
    maxR    <- H.forAll $ Gen.int (Range.linear (minR + 1) 200)
    version <- H.forAll $ D.mkNoteVersion <$> (Gen.int (Range.linear minR maxR))

    -- inc maxR so that we don't have an instance where version <= maxR (we need version < maxR)
    let range = D.versionRange (D.VersionRange minR (maxR + 1)) version
    case range of
      (D.ValidNoteVersionRange noteVersion) -> version H.=== noteVersion
      (D.InvalidNoteVersionRange _ _)       -> H.failure

hprop_versionRange_failure :: H.Property
hprop_versionRange_failure =
  H.property $ do
    minR  <- H.forAll $ Gen.int (Range.linear 0 99)
    maxR  <- H.forAll $ Gen.int (Range.linear (minR + 1) 200)
    let upperG :: H.Gen Int =  Gen.int (Range.linear maxR (maxR + 100))
        lowerG :: H.Gen Int =  Gen.int (Range.linear (minR - 100) minR)
        minMax = (D.VersionRange minR maxR)
    versionE <- H.forAll $ Gen.either lowerG upperG
    either (assertVersionRangeFailure minMax) (assertVersionRangeFailure minMax) versionE
      where
            assertVersionRangeFailure :: D.VersionRange -> Int -> H.PropertyT IO ()
            assertVersionRangeFailure maxMin version =
              let range = D.versionRange maxMin (D.mkNoteVersion version)
              in case range of
                  (D.InvalidNoteVersionRange v r) -> (r H.=== maxMin) >> (v H.=== version)
                  (D.ValidNoteVersionRange _)     -> H.failure


unit_determineUpdate_note_not_found :: Assertion
unit_determineUpdate_note_not_found =
  let dbNoteE = createDBNote (mkNoteId 1000) "blee" (mkNoteVersion 1)
  in case dbNoteE of
      Left x       -> assertFailure (show x)
      Right dbNote ->
        let result = determineUpdate dbNote [] undefined
        in
          case result of
            NoMatchingNoteFound noteId -> noteId @?= 1000
            other -> assertFailure $ "expected NoMatchingNoteFound but got: " <> (show other)

-- determineUpdate :: DBNote -> [NoteVersionFromDB] -> VersionRange -> UpdateAction