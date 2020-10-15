module SampleSpec where

import Test.Tasty.HUnit ((@?=), Assertion)

import qualified Hedgehog              as H
import qualified Hedgehog.Gen          as Gen
import qualified Hedgehog.Range        as Range

hprop_equality :: H.Property
hprop_equality =
  H.property $ do
    randomString <- H.forAll $ Gen.text (Range.linear 0 100) Gen.alpha
    randomString H.=== randomString

unit_equality :: Assertion
unit_equality =
  let actual   = 100 :: Int
      expected = 100 :: Int
  in actual @?= expected

