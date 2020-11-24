{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE DerivingStrategies  #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Model
       (
          -- DATA TYPES

          IncomingNote(..)
       ,  NoteIdAndVersion(..)
       ,  OutgoingNote(..)
       ,  ApiKey(..)
       ,  DBError(..)
       ,  OutgoingError(..)

         -- FUNCTIONS

       ,  getDBErrorCode
       ,  dbErrorToString
       ,  showt
       ,  outgoingErrorText
       ) where

import GHC.Generics
import Data.Aeson
import Data.Aeson.Types (Parser, parseFail)
import Data.Aeson.Text (encodeToLazyText)

import Data.Aeson.Casing (aesonDrop, camelCase)

import qualified Data.Text      as T
import qualified Data.Text.Lazy as LT


-- DATA TYPES

data NoteIdAndVersion =
  NoteIdAndVersion {
    _noteIdAndVersionNoteId      :: Int
  , _noteIdAndVersionNoteVersion :: Int
}  deriving stock (Generic, Eq, Show)

data OutgoingNote =
  OutgoingNote {
    _outgoingNoteText :: T.Text
  , _outgoingNoteId :: Int
  , _outgoingNoteVersion :: Int
  } deriving stock (Generic, Show)

data IncomingNote =
  IncomingNote {
    _incomingNoteText       :: T.Text
  , _incomingNoteAndVersion :: Maybe NoteIdAndVersion
  } deriving stock (Generic, Eq, Show)

data OutgoingError =
  OutgoingError {
    _outgoingErrorId :: Int
  , _outgoingErrorMessage :: T.Text
  } deriving stock (Generic, Show, Eq)

newtype ApiKey = ApiKey { _apiKey :: T.Text } deriving stock (Eq, Show)

data DBError = ItemNotFound Int
             | InvalidVersion Int
             | VersionMismatch Int Int
             | NoteTextIsEmpty deriving stock (Eq, Show)


getDBErrorCode :: DBError -> Int
getDBErrorCode (ItemNotFound _)      = 1000
getDBErrorCode (InvalidVersion _)    = 1001
getDBErrorCode (VersionMismatch _ _) = 1002
getDBErrorCode NoteTextIsEmpty       = 1004

dbErrorToString :: DBError -> OutgoingError
dbErrorToString db@(ItemNotFound _)      = OutgoingError (getDBErrorCode db) "The note specified could not be found"
dbErrorToString db@(InvalidVersion _)    = OutgoingError (getDBErrorCode db) "The version of the note supplied is invalid"
dbErrorToString db@(VersionMismatch _ _) = OutgoingError (getDBErrorCode db) "There's a different version of this note on the server. Refresh and try again"
dbErrorToString db@(NoteTextIsEmpty)     = OutgoingError (getDBErrorCode db) "The note supplied does not have any text. Please add some text and try again"


outgoingErrorText :: Int -> T.Text -> T.Text
outgoingErrorText code message =
  let errorObject = OutgoingError code message
      errorJson   = toJSON errorObject
      errorString = LT.toStrict . encodeToLazyText $ errorJson
  in errorString

-- JSON ENCODE/DECODE


outgoingJsonOptions :: Options
outgoingJsonOptions = aesonDrop 9 camelCase

noteAndVersionJsonOptions :: Options
noteAndVersionJsonOptions = aesonDrop 17 camelCase


instance ToJSON OutgoingNote where
   toEncoding = genericToEncoding outgoingJsonOptions

instance FromJSON OutgoingNote where
  parseJSON = genericParseJSON outgoingJsonOptions


instance FromJSON IncomingNote where
  parseJSON = withObject "IncomingNote" $ \v ->
    let noteTextP           = v .: "noteText"     :: Parser T.Text
        maybeNoteIdP        = v .:? "noteId"      :: Parser (Maybe Int)
        maybeNoteVersionP   = v .:? "noteVersion" :: Parser (Maybe Int)
        maybeNoteIdVersionP = do
                                maybeNoteId      <- maybeNoteIdP
                                maybeNoteVersion <- maybeNoteVersionP
                                case (maybeNoteId, maybeNoteVersion) of
                                  (Just noteId, Just noteVersion) -> pure $ Just $ NoteIdAndVersion noteId noteVersion
                                  (Nothing, Nothing)              -> pure $ Nothing
                                  _                               -> parseFail "You need to supply both 'noteId' and 'noteVersion' or omit both"

    in IncomingNote <$> noteTextP <*> maybeNoteIdVersionP

instance ToJSON NoteIdAndVersion where
   toEncoding = genericToEncoding noteAndVersionJsonOptions

instance FromJSON NoteIdAndVersion where
  parseJSON = genericParseJSON noteAndVersionJsonOptions

instance ToJSON OutgoingError where
   toEncoding = genericToEncoding outgoingJsonOptions


-- UTIL

showt :: Show a => a -> T.Text
showt = T.pack . show