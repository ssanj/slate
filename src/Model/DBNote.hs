{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE DerivingStrategies  #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Model.DBNote
       (
          -- Data types

          DBNote
       ,  NewDBNote
       ,  NoteId
       ,  NoteVersion
       ,  NoteIdVersion(..)
       ,  NoteText

          -- GETTERS

       ,  getDBNote
       ,  getNoteText
       ,  getNewDBNoteText

        -- CONSTRUCTORS

       ,  createNoteText
       ,  createDBNote
       ,  mkNoteId
       ,  mkNoteVersion
       ,  mkNewDBNote
       ) where

import Prelude hiding (null)
import Data.Text
import Data.Aeson

import Database.SQLite.Simple (ToRow(..), FromRow(..), SQLData(SQLText), field)

import Data.Tagged (Tagged(..))
import Model (DBError(NoteTextIsEmpty))

data NoteIdTag
data VersionTag

type TInt s = Tagged s Int

type NoteId = TInt NoteIdTag
type NoteVersion = TInt VersionTag

newtype NoteText = NoteText Text deriving stock (Eq, Show)

data NoteIdVersion =
  NoteIdVersion {
    _noteIdVersionNoteId :: NoteId
  , _noteIdVersionVersion :: NoteVersion
  } deriving stock (Eq, Show)

data DBNote = DBNote { _dbNoteId :: Int, _dbNoteText :: Text, _dbNoteVersion :: Int } deriving stock (Eq, Show)

newtype NewDBNote = NewDBNote {  _newdbNoteText ::NoteText } deriving stock (Eq, Show)

mkNewDBNote :: Text -> Either DBError NewDBNote
mkNewDBNote noteText = NewDBNote <$> createNoteText noteText

mkNoteId :: Int -> NoteId
mkNoteId = Tagged

mkNoteVersion :: Int -> NoteVersion
mkNoteVersion = Tagged

getDBNote :: DBNote -> (NoteId, NoteText, NoteVersion)
getDBNote (DBNote noteId noteText noteVersion) = ((Tagged noteId), (NoteText noteText), (Tagged noteVersion))

getNoteText :: NoteText -> Text
getNoteText (NoteText noteText) = noteText

getNewDBNoteText :: NewDBNote -> Text
getNewDBNoteText (NewDBNote noteText) = getNoteText noteText

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
  toJSON (NoteIdVersion noteId noteVersion) =
    object
      [
        "noteId"      .= noteId
      , "noteVersion" .= noteVersion
      ]

-- Only allow going to the db without an id, not the other way around
instance ToRow NewDBNote where
  toRow (NewDBNote (NoteText message_)) = [SQLText message_]