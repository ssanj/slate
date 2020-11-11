{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module DBSpec where

import qualified Hedgehog              as H
import qualified Hedgehog.Gen          as Gen
import qualified Hedgehog.Range        as Range
import qualified Model.DBNote          as D

hprop_versionRange :: H.Property
hprop_versionRange =
  H.property $ do
    minR    <- H.forAll $ Gen.int (Range.linear 0 99)
    maxR    <- H.forAll $ Gen.int (Range.linear (minR + 1) 200)
    version <- H.forAll $ D.mkNoteVersion <$> (Gen.int (Range.linear minR maxR))

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
