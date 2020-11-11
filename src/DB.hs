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
    let (noteId, _, _) = getDBNote dbNote
    versions <-  query con "SELECT VERSION FROM SCRIB WHERE ID = ?" (Only noteId) :: IO [Only NoteVersionFromDB]
    case versions of
      []    -> pure . Left $ ItemNotFound (getInt noteId)
      ((Only oldVersion):_) ->

        let updateAction = determineUpdate dbNote oldVersion (VersionRange minVersion maxVersion)
        in
          case updateAction of
            (DoUpdate noteId_ noteMessage dbVersion newVersion) ->
              updateNote noteId_ noteMessage dbVersion newVersion con >>
                (pure . Right $ mkUpdatedNoteIdVersion noteId_ newVersion)

            (VersionMismatchError v1 v2) ->
              pure . Left $ VersionMismatch (getInt v1) (getInt v2)

            (InvalidVersionRangeError version) ->
              pure . Left $ InvalidVersion version


updateNote :: NoteId -> NoteText -> NoteVersionFromDB  -> UpdatedNoteVersion -> Connection -> IO ()
updateNote noteId noteMessage dbVersion newVersion con =
  executeNamed con
    "UPDATE SCRIB SET MESSAGE = :message, VERSION = :newVersion WHERE ID = :id and VERSION = :oldVersion"
      [
        ":message"    := noteMessage
      , ":id"         := noteId
      , ":oldVersion" := dbVersion
      , ":newVersion" := newVersion
      ]


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
