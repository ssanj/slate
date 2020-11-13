{-# LANGUAGE OverloadedStrings #-}

module IncomingNoteSpec where

import Test.Tasty.HUnit                (assertFailure, (@?=), Assertion)

import Data.Aeson
import Model (IncomingNote(..))
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
      (Success value) -> value @?= (IncomingNote "blah" Nothing Nothing)

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
      (Success value) -> value @?= (IncomingNote "blah" (Just 1000) (Just 1))
