{-# OPTIONS_GHC -fno-warn-orphans #-}

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
       ,  NoteIdVersion
       ,  NoteText
       ,  NoteVersionRange(..)
       ,  VersionRange(..)
       ,  NoteVersionEquality(..)

          -- GETTERS

       ,  getDBNote
       ,  getNoteText
       ,  getNewDBNoteText
       ,  getDBNoteText
       ,  getNoteId
       ,  getNoteVersion
       ,  getDBNoteId
       ,  getDBNoteVersion
       ,  getInt

        -- CONSTRUCTORS

       ,  createNoteText
       ,  createDBNote
       ,  mkNoteId
       ,  mkNoteVersion
       ,  mkNewDBNote
       ,  mkNoteIdVersion


         -- UTIL

       ,  versionRange
       ,  sameNoteVersion

       ) where

import Prelude hiding (null)
import Data.Text
import Data.Aeson

import Database.SQLite.Simple (ToRow(..), FromRow(..), SQLData(SQLText), field)
import Database.SQLite.Simple.ToField (ToField(..))
import Database.SQLite.Simple.FromField (FromField(..))
import Data.Tagged (Tagged(..), untag)
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

data NoteVersionRange = ValidNoteVersionRange NoteVersion
                      | InvalidNoteVersionRange Int VersionRange deriving stock (Eq, Show)

data VersionRange =
  VersionRange {
    versionRangeMin :: Int
  , versionRangeMax :: Int
  } deriving stock (Eq, Show)

versionRange :: VersionRange -> NoteVersion -> NoteVersionRange
versionRange (VersionRange minR  maxR) noteVersion =
  let version = untag noteVersion
  in
    if version >= minR && version < maxR then ValidNoteVersionRange noteVersion
    else InvalidNoteVersionRange version (VersionRange minR maxR)


data NoteVersionEquality = SameNoteVersion NoteVersion
                         | DifferentNoteVersions NoteVersion NoteVersion deriving stock (Eq, Show)

sameNoteVersion :: NoteVersion -> NoteVersion -> NoteVersionEquality
sameNoteVersion srcNoteVersion targetNoteVersion =
  if srcNoteVersion == targetNoteVersion then SameNoteVersion srcNoteVersion
  else DifferentNoteVersions srcNoteVersion targetNoteVersion


mkNoteIdVersion :: NoteId -> NoteVersion -> NoteIdVersion
mkNoteIdVersion = NoteIdVersion

mkNewDBNote :: Text -> Either DBError NewDBNote
mkNewDBNote noteText = NewDBNote <$> createNoteText noteText

mkNoteId :: Int -> NoteId
mkNoteId = Tagged

mkNoteVersion :: Int -> NoteVersion
mkNoteVersion = Tagged

getInt :: TInt a -> Int
getInt = untag

getDBNote :: DBNote -> (NoteId, NoteText, NoteVersion)
getDBNote (DBNote noteId noteText noteVersion) = ((Tagged noteId), (NoteText noteText), (Tagged noteVersion))

getNoteText :: NoteText -> Text
getNoteText (NoteText noteText) = noteText

getDBNoteText :: DBNote -> Text
getDBNoteText dbNote =
  let (_, note, _) = getDBNote dbNote
  in getNoteText note

getDBNoteId :: DBNote -> Int
getDBNoteId = _dbNoteId

getDBNoteVersion:: DBNote -> Int
getDBNoteVersion= _dbNoteVersion

getNewDBNoteText :: NewDBNote -> Text
getNewDBNoteText (NewDBNote noteText) = getNoteText noteText

createDBNote :: NoteId -> Text -> NoteVersion -> Either DBError DBNote
createDBNote noteId noteText noteVersion =
  (\(NoteText validNoteText) -> DBNote (unTagged noteId) validNoteText (unTagged noteVersion)) <$> (createNoteText noteText)

createNoteText :: Text -> Either DBError NoteText
createNoteText noteText =
  if null noteText then Left NoteTextIsEmpty
  else Right $ NoteText noteText

getNoteId :: NoteIdVersion -> Int
getNoteId = untag . _noteIdVersionNoteId

getNoteVersion :: NoteIdVersion -> Int
getNoteVersion = untag . _noteIdVersionVersion

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

instance ToField NoteText where
  toField (NoteText noteText)= toField noteText

instance ToField a => ToField (Tagged x a) where
  toField = toField . untag

instance FromField a => FromField (Tagged x a) where
  fromField = fmap Tagged . fromField

