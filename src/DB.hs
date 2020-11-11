{-# LANGUAGE OverloadedStrings   #-}

module DB
       (
          -- Data types
          FetchSize
          -- Functions
       ,  saveExitingNote
       ,  saveNewNote
       ,  fetchNotes
       ,  fetchSize
       ,  searchNotes
       ,  maxFetchSize
       ) where

import Model
import Model.DBNote
import Database.SQLite.Simple

import Data.Tagged (untag)
import Control.Applicative (liftA2)
import qualified Data.Text  as T

-- We don't expect more than a 1000 updates.
maxVersion :: Int
maxVersion = 1000

-- This is the lowest version we expect
minVersion :: Int
minVersion = 1

--TODO: Run this in a transaction
saveExitingNote :: DBNote -> Connection -> IO (Either DBError NoteIdVersion)
saveExitingNote dbNote con = do
    let (noteId, noteMessage, noteVersion) = getDBNote dbNote
        id_      = untag noteId
        message_ = getNoteText noteMessage
    versions <-  query con "SELECT VERSION FROM SCRIB WHERE ID = ?" (Only (id_ :: Int)) :: IO [Only NoteVersion]
    case versions of
      []    -> pure . Left $ ItemNotFound id_
      ((Only oldVersion):_) ->
        let validVersionRange   = versionRange (VersionRange minVersion maxVersion) noteVersion -- one less than max to allow for one final increment
            noteVersionEquality = sameNoteVersion oldVersion noteVersion
        in
          case (validVersionRange, noteVersionEquality) of
            ((ValidNoteVersionRange version), (SameNoteVersion _)) ->
              do
                let newVersion = (+ 1) <$> version
                executeNamed con
                  "UPDATE SCRIB SET MESSAGE = :message, VERSION = :newVersion WHERE ID = :id and VERSION = :oldVersion"
                    [
                      ":message" := message_
                    , ":id" := noteId
                    , ":oldVersion" := oldVersion
                    , ":newVersion" := newVersion
                    ]
                pure . Right $ mkNoteIdVersion noteId newVersion

            ((ValidNoteVersionRange _), (DifferentNoteVersions v1 v2))         -> pure . Left $ VersionMismatch (getInt v1) (getInt v2)
            ((InvalidNoteVersionRange version _), (SameNoteVersion _ ))        -> pure . Left $ InvalidVersion version
            ((InvalidNoteVersionRange version _), (DifferentNoteVersions _ _)) -> pure . Left $ InvalidVersion version


saveNewNote :: NewDBNote -> Connection -> IO NoteIdVersion
saveNewNote newDBNote con = do
  let message = getNewDBNoteText newDBNote
  execute con "INSERT INTO SCRIB (MESSAGE) VALUES (?)" (Only (message :: T.Text))
  let noteIdIO       = (pure . fromIntegral . toInteger <$> (lastInsertRowId con)) :: IO NoteId
      noteVersionIO  = (pure . pure $ minVersion) :: IO NoteVersion
  liftA2 mkNoteIdVersion noteIdIO noteVersionIO

newtype FetchSize = FetchSize { _fetchSize :: Int }

fetchSize :: Int -> FetchSize
fetchSize n
  | n > 0 && n <= maxSize = FetchSize n
  | otherwise             = maxFetchSize

maxFetchSize :: FetchSize
maxFetchSize = FetchSize maxSize

maxSize :: Int
maxSize = 50

fetchNotes :: FetchSize -> Connection -> IO [DBNote]
fetchNotes (FetchSize size) con = query con "SELECT ID, MESSAGE, VERSION FROM SCRIB WHERE MESSAGE <> '' ORDER BY UPDATED_AT DESC LIMIT (?)" (Only size):: IO [DBNote]

searchNotes :: T.Text -> Connection -> IO [DBNote]
searchNotes searchCriteria con = query con "SELECT ID, MESSAGE, VERSION FROM SCRIB WHERE MESSAGE LIKE (?) ORDER BY UPDATED_AT DESC" (Only ("%" <> searchCriteria <> "%")) :: IO [DBNote]
