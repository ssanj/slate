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
       ) where

import GHC.Generics
import Data.Aeson

import Data.Aeson.Casing (aesonDrop, camelCase)
import Database.SQLite.Simple (ToRow(..), FromRow(..), field, SQLData(..))

import qualified Data.Text                     as T

-- Data types

data OutgoingNote = OutgoingNote { _outgoingNoteText :: T.Text, _outgoingNoteId :: Int } deriving stock (Generic, Show)

data IncomingNote = IncomingNote { _incomingNoteText :: T.Text, _incomingNoteId :: Maybe Int} deriving stock (Generic, Show)

data DBNote = DBNote {  _dbNoteId :: Int, _dbNoteText :: T.Text } deriving stock (Show)

newtype NewDBNote = NewDBNote {  _newdbNoteText :: T.Text } deriving stock (Show)

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


-- DB FromRow/ToRow

instance FromRow DBNote where
  fromRow = DBNote <$> field <*> field

instance ToRow DBNote where
  toRow (DBNote id_ message_) = toRow (id_, message_)

-- Only allow going to the db without an id, not the other way around
instance ToRow NewDBNote where
  toRow (NewDBNote message_) = [SQLText message_]

