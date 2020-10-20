{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE DerivingStrategies  #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Model
       (
          -- Data types
          IncomingNote(..)
       ,  OutgoingNote(..)
       ,  NewDBNote(..)
       ,  DBNote(..)
       ,  ApiKey(..)
       ,  DBError(..)
       ,  OutgoingError(..)
       ,  NoteIdVersion(..)
       ,  NoteId
       ,  NoteVersion

         -- Functions
       ,  getDBErrorCode
       ) where

import GHC.Generics
import Data.Aeson
import Data.Tagged (Tagged(..))

import Data.Aeson.Casing (aesonDrop, camelCase)
import Database.SQLite.Simple (ToRow(..), FromRow(..), field, SQLData(..))

import qualified Data.Text                     as T

-- Data types

data OutgoingNote = OutgoingNote { _outgoingNoteText :: T.Text, _outgoingNoteId :: Int, _outgoingNoteVersion :: Int } deriving stock (Generic, Show)

data IncomingNote = IncomingNote { _incomingNoteText :: T.Text, _incomingNoteId :: Maybe Int, _incomingNoteVersion :: Maybe Int } deriving stock (Generic, Show)

data OutgoingError = OutgoingError { _outgoingErrorId :: Int, _outgoingErrorMessage :: T.Text } deriving stock (Generic, Show)

data DBNote = DBNote {  _dbNoteId :: Int, _dbNoteText :: T.Text, _dbNoteVersion :: Int } deriving stock (Show)

newtype NewDBNote = NewDBNote {  _newdbNoteText :: T.Text } deriving stock (Show)

newtype ApiKey = ApiKey { _apiKey :: T.Text } deriving stock (Eq, Show)

type TInt s = Tagged s Int

data NoteIdTag
data VersionTag

type NoteId = TInt NoteIdTag
type NoteVersion = TInt VersionTag

data NoteIdVersion = NoteIdVersion { _noteIdVersionNoteId :: NoteId, _noteIdVersionVersion :: NoteVersion }

data DBError = ItemNotFound Int
             | InvalidVersion Int
             | VersionMismatch Int Int
             | NeedIdAndVersion deriving stock (Eq, Show)

getDBErrorCode :: DBError -> Int
getDBErrorCode (ItemNotFound _)      = 1000
getDBErrorCode (InvalidVersion _)    = 1001
getDBErrorCode (VersionMismatch _ _) = 1002
getDBErrorCode NeedIdAndVersion      = 1003


-- JSON Encode/Decode

outgoingJsonOptions :: Options
outgoingJsonOptions = aesonDrop 9 camelCase

incomingJsonOptions :: Options
incomingJsonOptions = aesonDrop 9 camelCase


instance ToJSON OutgoingNote where
   toEncoding = genericToEncoding outgoingJsonOptions

instance FromJSON OutgoingNote where
  parseJSON = genericParseJSON outgoingJsonOptions


instance ToJSON IncomingNote where
   toEncoding = genericToEncoding incomingJsonOptions

instance FromJSON IncomingNote where
  parseJSON = genericParseJSON incomingJsonOptions

instance ToJSON OutgoingError where
   toEncoding = genericToEncoding outgoingJsonOptions

instance ToJSON NoteIdVersion where
  toJSON noteIdVersion =
    object
      [
        "noteId"      .= (unTagged . _noteIdVersionNoteId $ noteIdVersion :: Int)
      , "noteVersion" .= (unTagged . _noteIdVersionVersion $ noteIdVersion :: Int)
      ]

-- DB FromRow/ToRow

instance FromRow DBNote where
  fromRow = DBNote <$> field <*> field <*> field

instance ToRow DBNote where
  toRow (DBNote id_ message_ version_) = toRow (id_, message_, version_)

-- Only allow going to the db without an id, not the other way around
instance ToRow NewDBNote where
  toRow (NewDBNote message_) = [SQLText message_]


