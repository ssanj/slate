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
       ,  ApiKey(..)
       ,  DBError(..)
       ,  OutgoingError(..)

         -- Functions
       ,  getDBErrorCode
       ,  dbErrorToString
       ) where

import GHC.Generics
import Data.Aeson

import Data.Aeson.Casing (aesonDrop, camelCase)
import Database.SQLite.Simple (ToRow(..), SQLData(..))

import qualified Data.Text                     as T

-- Data types

data OutgoingNote = OutgoingNote { _outgoingNoteText :: T.Text, _outgoingNoteId :: Int, _outgoingNoteVersion :: Int } deriving stock (Generic, Show)

data IncomingNote = IncomingNote { _incomingNoteText :: T.Text, _incomingNoteId :: Maybe Int, _incomingNoteVersion :: Maybe Int } deriving stock (Generic, Show)

data OutgoingError = OutgoingError { _outgoingErrorId :: Int, _outgoingErrorMessage :: T.Text } deriving stock (Generic, Show)

newtype NewDBNote = NewDBNote {  _newdbNoteText :: T.Text } deriving stock (Show)

newtype ApiKey = ApiKey { _apiKey :: T.Text } deriving stock (Eq, Show)

data DBError = ItemNotFound Int
             | InvalidVersion Int
             | VersionMismatch Int Int
             | NoteTextIsEmpty
             | NeedIdAndVersion deriving stock (Eq, Show)

getDBErrorCode :: DBError -> Int
getDBErrorCode (ItemNotFound _)      = 1000
getDBErrorCode (InvalidVersion _)    = 1001
getDBErrorCode (VersionMismatch _ _) = 1002
getDBErrorCode NeedIdAndVersion      = 1003
getDBErrorCode NoteTextIsEmpty       = 1004

dbErrorToString :: DBError -> OutgoingError
dbErrorToString db@(ItemNotFound _)      = OutgoingError (getDBErrorCode db) "The note specified could not be found"
dbErrorToString db@(InvalidVersion _)    = OutgoingError (getDBErrorCode db) "The version of the note supplied is invalid"
dbErrorToString db@NeedIdAndVersion      = OutgoingError (getDBErrorCode db) "The save did not send the expected information to the server"
dbErrorToString db@(VersionMismatch _ _) = OutgoingError (getDBErrorCode db) "There's a different version of this note on the server. Refresh and try again"
dbErrorToString db@(NoteTextIsEmpty)     = OutgoingError (getDBErrorCode db) "The note supplied does not have any text. Please add some text and try again"

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

-- Only allow going to the db without an id, not the other way around
instance ToRow NewDBNote where
  toRow (NewDBNote message_) = [SQLText message_]


