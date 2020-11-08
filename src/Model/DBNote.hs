{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE DerivingStrategies  #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Model.DBNote
       (
          -- Data types

          DBNote
       ,  NoteId
       ,  NoteVersion
       ,  NoteIdVersion(..)
       ,  NoteText

          -- GETTERS

       ,  getDBNote
       ,  getNoteText

        -- CONSTRUCTORS

       ,  createNoteText
       ,  createDBNote
       ,  mkNoteId
       ,  mkNoteVersion
       ) where

import Prelude hiding (null)
import Data.Text
import Data.Aeson

import Database.SQLite.Simple (ToRow(..), FromRow(..), field)
import Data.Tagged (Tagged(..))
import Model (DBError(NoteTextIsEmpty))

data NoteIdTag
data VersionTag

type TInt s = Tagged s Int

type NoteId = TInt NoteIdTag
type NoteVersion = TInt VersionTag

data NoteText = NoteText Text

data NoteIdVersion = NoteIdVersion { _noteIdVersionNoteId :: NoteId, _noteIdVersionVersion :: NoteVersion }

data DBNote = DBNote {  _dbNoteId :: Int, _dbNoteText :: Text, _dbNoteVersion :: Int } deriving stock (Show)

mkNoteId :: Int -> NoteId
mkNoteId = Tagged

mkNoteVersion :: Int -> NoteVersion
mkNoteVersion = Tagged

getDBNote :: DBNote -> (NoteId, NoteText, NoteVersion)
getDBNote (DBNote noteId noteText noteVersion) = ((Tagged noteId), (NoteText noteText), (Tagged noteVersion))

getNoteText :: NoteText -> Text
getNoteText (NoteText noteText) = noteText

createDBNote :: NoteId -> Text -> NoteVersion -> Either DBError DBNote
createDBNote noteId noteText noteVersion =
  (\(NoteText validNoteText) -> DBNote (unTagged noteId) validNoteText (unTagged noteVersion)) <$> (createNoteText noteText)

createNoteText :: Text -> Either DBError NoteText
createNoteText noteText =
  if null noteText then Left NoteTextIsEmpty
  else Right $ NoteText noteText

-- DB FromRow/ToRow

instance FromRow DBNote where
  fromRow = DBNote <$> field <*> field <*> field

instance ToRow DBNote where
  toRow (DBNote id_ message_ version_) = toRow (id_, message_, version_)

instance ToJSON NoteIdVersion where
  toJSON noteIdVersion =
    object
      [
        "noteId"      .= (unTagged . _noteIdVersionNoteId $ noteIdVersion :: Int)
      , "noteVersion" .= (unTagged . _noteIdVersionVersion $ noteIdVersion :: Int)
      ]
