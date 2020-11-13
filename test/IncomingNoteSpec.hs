{-# LANGUAGE OverloadedStrings #-}

module IncomingNoteSpec where

import Test.Tasty.HUnit                (assertFailure, (@?=), Assertion)

import Data.Aeson
import Model (IncomingNote(..), NoteIdAndVersion(..))
import qualified Data.Text    as T


unit_your_decodes_IncomingNote :: Assertion
unit_your_decodes_IncomingNote =
  let incomingJSON =
        object
          [
            "noteText" .= ("blah" :: T.Text)
          ]
      result     = (fromJSON incomingJSON :: Result IncomingNote)
  in
    case result of
      (Error x)       -> assertFailure $ "Decoding failed with: " <> x
      (Success value) -> value @?= (IncomingNote "blah" Nothing)


unit_your_decodes_IncomingNote_with_version_id :: Assertion
unit_your_decodes_IncomingNote_with_version_id =
  let incomingJSON =
        object
          [
            "noteText"     .= ("blah" :: T.Text)
          , "noteId"       .= (1000  :: Int)
          , "noteVersion" .=  (1 :: Int)
          ]
      result     = (fromJSON incomingJSON :: Result IncomingNote)
  in
    case result of
      (Error x)       -> assertFailure $ "Decoding failed with: " <> x
      (Success value) -> value @?= (IncomingNote "blah" (Just $ NoteIdAndVersion 1000 1))


unit_your_decodes_IncomingNote_without_noteId :: Assertion
unit_your_decodes_IncomingNote_without_noteId =
  assert_should_not_have_noteId_or_noteVersion $
    object
      [
        "noteText"     .= ("blah" :: T.Text)
      , "noteVersion" .=  (1 :: Int)
      ]


unit_your_decodes_IncomingNote_without_noteVersion :: Assertion
unit_your_decodes_IncomingNote_without_noteVersion =
  assert_should_not_have_noteId_or_noteVersion $
    object
      [
        "noteText"     .= ("blah" :: T.Text)
      , "noteId"       .= (1000  :: Int)
      ]


assert_should_not_have_noteId_or_noteVersion :: Value -> Assertion
assert_should_not_have_noteId_or_noteVersion jsonValue =
  let result = (fromJSON jsonValue :: Result IncomingNote)
  in
    case result of
      (Error x)       -> x @?= "You need to supply both 'noteId' and 'noteVersion' or omit both"
      (Success value) -> assertFailure $ "Expected decoding failure but got: " <> (show value)
