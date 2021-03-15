{-# LANGUAGE OverloadedStrings   #-}

module DB.Repository
       (
          -- Data types
          FetchSize
          -- Functions
       ,  saveExistingNote
       ,  saveNewNote
       ,  fetchNotes
       ,  fetchSize
       ,  searchNotes
       ,  deactivateNote
       ,  maxFetchSize
       ) where

import Model
import DB.DBNote
import Database.SQLite.Simple
import Data.Maybe (listToMaybe)

import Control.Applicative (liftA2)
import qualified Data.Text  as T

-- How many records to fetch when calling `fetch`
newtype FetchSize = FetchSize { _fetchSize :: Int }

-- We don't expect more than a 1000 updates.
maxVersion :: Int
maxVersion = 1000


-- This is the lowest version we expect
minVersion :: Int
minVersion = 1


fetchSize :: Int -> FetchSize
fetchSize n
  | n > 0 && n <= maxSize = FetchSize n
  | otherwise             = maxFetchSize


maxFetchSize :: FetchSize
maxFetchSize = FetchSize maxSize


-- Maxium number of records to fetch
maxSize :: Int
maxSize = 50


-- Run this within a transaction
saveExistingNote :: DBNote -> Connection -> IO (Either DBError NoteIdVersion)
saveExistingNote dbNote con = do
    let (noteId, _, _) = getDBNote dbNote
    maybeDbVersionAndDeleteFlag <- getNoteVersionAndDeleteStatusFromDB noteId con
    let updateAction = determineUpdate dbNote maybeDbVersionAndDeleteFlag (VersionRange minVersion maxVersion)
    case updateAction of
      NoMatchingNoteFound noteId_ -> pure . Left $ ItemNotFound (getInt noteId_)

      (DoUpdate noteId_ noteMessage dbVersion newVersion) ->
        updateNote noteId_ noteMessage dbVersion newVersion con >>
          (pure . Right $ mkUpdatedNoteIdVersion noteId_ newVersion)

      (VersionMismatchError v1 v2) ->
        pure . Left $ VersionMismatch (getInt v1) (getInt v2)

      (InvalidVersionRangeError version) ->
        pure . Left $ InvalidVersion version

      CantUpdateDeletedNote -> pure . Left $ UpdatingDeletedNote (getInt noteId)


getNoteVersionAndDeleteStatusFromDB :: NoteId -> Connection -> IO (Maybe NoteVersionAndDeletedFromDB)
getNoteVersionAndDeleteStatusFromDB noteId con =
  let resultIO = query con "SELECT VERSION, DELETED FROM SCRIB WHERE ID = ?" (Only noteId) :: IO [NoteVersionAndDeletedFromDB]
  in listToMaybe <$> resultIO

getNoteIdAndDeleteStatusFromDB :: NoteId -> Connection -> IO (Maybe NoteIdAndDeletedFromDB)
getNoteIdAndDeleteStatusFromDB noteId con =
  let resultIO = query con "SELECT ID, DELETED FROM SCRIB WHERE ID = ?" (Only noteId) :: IO [NoteIdAndDeletedFromDB]
  in listToMaybe <$> resultIO


updateNote :: NoteId -> NoteText -> NoteVersionFromDB  -> UpdatedNoteVersion -> Connection -> IO ()
updateNote noteId noteMessage dbVersion newVersion con =
  executeNamed con
    "UPDATE SCRIB SET MESSAGE = :message, VERSION = :newVersion WHERE ID = :id AND VERSION = :oldVersion AND DELETED != 1"
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


fetchNotes :: FetchSize -> Connection -> IO [DBNote]
fetchNotes (FetchSize size) con = query con "SELECT ID, MESSAGE, VERSION FROM SCRIB WHERE MESSAGE <> '' AND DELETED = 0 ORDER BY UPDATED_AT DESC LIMIT (?)" (Only size):: IO [DBNote]


searchNotes :: T.Text -> Connection -> IO [DBNote]
searchNotes searchCriteria con = query con "SELECT ID, MESSAGE, VERSION FROM SCRIB WHERE MESSAGE LIKE (?) AND DELETED = 0 ORDER BY UPDATED_AT DESC" (Only ("%" <> searchCriteria <> "%")) :: IO [DBNote]


deactivateNote :: NoteId -> Connection -> IO (Either DBError OnlyNoteId)
deactivateNote noteId con = do
    maybeDbIdAndDeleteFlag <- getNoteIdAndDeleteStatusFromDB noteId con
    let deleteAction = determineDelete noteId maybeDbIdAndDeleteFlag
    case deleteAction of
      DoDelete _           -> (Right . OnlyNoteId . getInt $ noteId) <$ deleteNote noteId con
      NoteNotFound _       -> pure . Left . ItemNotFound . getInt $ noteId
      NoteAlreadyDeleted _ -> pure . Left . DeletingDeletedNote . getInt $ noteId


deleteNote :: NoteId -> Connection -> IO ()
deleteNote noteId con =
  executeNamed con
    "UPDATE SCRIB SET DELETED = 1 WHERE ID = :id AND DELETED != 1"
      [
        ":id"         := noteId
      ]
