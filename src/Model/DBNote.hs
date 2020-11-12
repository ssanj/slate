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
       ,  NoteVersionFromDB
       ,  UpdatedNoteVersion
       ,  NoteIdVersion
       ,  NoteText
       ,  NoteVersionRange(..)
       ,  VersionRange(..)
       ,  NoteVersionEquality(..)
       ,  UpdateAction(..)

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
       ,  getAnyVersion
       ,  getOutgoingNote

        -- CONSTRUCTORS

       ,  createNoteText
       ,  createDBNote
       ,  mkNoteId
       ,  mkNoteVersion
       ,  mkNewDBNote
       ,  mkNoteIdVersion
       ,  mkUpdatedNoteIdVersion
       ,  mkNoteVersionFromDB

         -- UTIL

       ,  versionRange
       ,  sameNoteVersion
       ,  determineUpdate

       ) where

import Prelude hiding (null)
import Data.Text
import Data.Aeson

import Database.SQLite.Simple (ToRow(..), FromRow(..), SQLData(SQLText), field)
import Database.SQLite.Simple.ToField (ToField(..))
import Database.SQLite.Simple.FromField (FromField(..))
import Data.Tagged (Tagged(..), untag, retag)
import Model (DBError(NoteTextIsEmpty), OutgoingNote(..))

data NoteIdTag
data VersionTag
data UpdatedVersionTag
data NoteVersionFromDBTag

type TInt s = Tagged s Int

type NoteId = TInt NoteIdTag
type NoteVersion = TInt VersionTag
type UpdatedNoteVersion = TInt UpdatedVersionTag
type NoteVersionFromDB = TInt NoteVersionFromDBTag

newtype NoteText = NoteText Text deriving stock (Eq, Show)

data NoteIdVersion =
  NoteIdVersion {
    _noteIdVersionNoteId :: NoteId
  , _noteIdVersionVersion :: NoteVersion
  } deriving stock (Eq, Show)

-- TODO: We could simply use NoteId, NoteText and NoteVersion here
data DBNote = DBNote { _dbNoteId :: Int, _dbNoteText :: Text, _dbNoteVersion :: Int } deriving stock (Eq, Show)

newtype NewDBNote = NewDBNote {  _newdbNoteText ::NoteText } deriving stock (Eq, Show)

data NoteVersionRange = ValidNoteVersionRange NoteVersion
                      | InvalidNoteVersionRange Int VersionRange deriving stock (Eq, Show)

data VersionRange =
  VersionRange {
    versionRangeMin :: Int
  , versionRangeMax :: Int
  } deriving stock (Eq, Show)

data NoteVersionEquality = SameNoteVersion NoteVersion
                         | DifferentNoteVersions NoteVersion NoteVersion deriving stock (Eq, Show)

data UpdateAction = DoUpdate NoteId NoteText NoteVersionFromDB UpdatedNoteVersion
                  | VersionMismatchError NoteVersion NoteVersion
                  | InvalidVersionRangeError Int
                  | NoMatchingNoteFound NoteId deriving stock (Eq, Show)


versionRange :: VersionRange -> NoteVersion -> NoteVersionRange
versionRange (VersionRange minR  maxR) noteVersion =
  let version = untag noteVersion
  in
    if version >= minR && version < maxR then ValidNoteVersionRange noteVersion
    else InvalidNoteVersionRange version (VersionRange minR maxR)


sameNoteVersion :: NoteVersion -> NoteVersion -> NoteVersionEquality
sameNoteVersion srcNoteVersion targetNoteVersion =
  if srcNoteVersion == targetNoteVersion then SameNoteVersion srcNoteVersion
  else DifferentNoteVersions srcNoteVersion targetNoteVersion

determineUpdate :: DBNote -> [NoteVersionFromDB] -> VersionRange -> UpdateAction
determineUpdate dbNote [] _                          = NoMatchingNoteFound (mkNoteId . getDBNoteId $ dbNote)
determineUpdate dbNote (dbVersion : _) versionLimits =
  let (noteId, noteMessage, noteVersion)             = getDBNote dbNote
      validVersionRange                              = versionRange versionLimits noteVersion
      noteVersionEquality                            = sameNoteVersion (retag dbVersion) noteVersion
  in
    case (validVersionRange, noteVersionEquality) of
      ((ValidNoteVersionRange version),     (SameNoteVersion _))           -> DoUpdate noteId noteMessage dbVersion (retag $ (+1) <$> version)
      ((ValidNoteVersionRange _),           (DifferentNoteVersions v1 v2)) -> VersionMismatchError v1 v2
      ((InvalidNoteVersionRange version _), (SameNoteVersion _ ))          -> InvalidVersionRangeError version
      ((InvalidNoteVersionRange version _), (DifferentNoteVersions _ _))   -> InvalidVersionRangeError version

mkNoteIdVersion :: NoteId -> NoteVersion -> NoteIdVersion
mkNoteIdVersion = NoteIdVersion

mkUpdatedNoteIdVersion :: NoteId -> UpdatedNoteVersion -> NoteIdVersion
mkUpdatedNoteIdVersion nid = mkNoteIdVersion nid . retag

mkNewDBNote :: Text -> Either DBError NewDBNote
mkNewDBNote noteText = NewDBNote <$> createNoteText noteText

mkNoteId :: Int -> NoteId
mkNoteId = Tagged

mkNoteVersion :: Int -> NoteVersion
mkNoteVersion = Tagged

mkNoteVersionFromDB :: Int -> NoteVersionFromDB
mkNoteVersionFromDB = Tagged

getInt :: TInt a -> Int
getInt = untag

getAnyVersion :: TInt a -> Int
getAnyVersion = untag

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

getOutgoingNote :: DBNote -> OutgoingNote
getOutgoingNote dbNote =
  let (noteId, noteText, noteVersion) = getDBNote dbNote
  in OutgoingNote (getNoteText noteText) (getInt noteId) (getInt noteVersion)

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

