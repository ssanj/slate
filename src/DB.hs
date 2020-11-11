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
        version_ = untag noteVersion
        message_ = getNoteText noteMessage
    -- let versions = [1] :: [Int]
    versions <-  query con "SELECT VERSION FROM SCRIB WHERE ID = ?" (Only (id_ :: Int)) :: IO [Only Int]
       -- queryNamed con "SELECT VERSION FROM SCRIB WHERE ID = :id" [":id" := id_]
    case versions of
      []    -> pure . Left $ ItemNotFound id_
      ((Only oldVersion):_) ->
        let validVersionRange   = versionRange (minVersion, maxVersion) noteVersion -- one less than max to allow for one final increment
            sameVersionAsClient = oldVersion == version_
        in
          case (validVersionRange, sameVersionAsClient) of
            ((ValidNoteVersionRange version), True) ->
              do
                let newVersion = (untag version) + 1
                executeNamed con
                  "UPDATE SCRIB SET MESSAGE = :message, VERSION = :newVersion WHERE ID = :id and VERSION = :oldVersion"
                    [
                      ":message" := message_
                    , ":id" := id_
                    , ":oldVersion" := oldVersion
                    , ":newVersion" := newVersion
                    ]
                pure . Right $ NoteIdVersion (pure id_) (pure newVersion)

            ((ValidNoteVersionRange version), False)     -> pure . Left $ VersionMismatch oldVersion (untag version)
            ((InvalidNoteVersionRange version _), True)  -> pure . Left $ InvalidVersion version
            ((InvalidNoteVersionRange version _), False) -> pure . Left $ InvalidVersion version


saveNewNote :: NewDBNote -> Connection -> IO NoteIdVersion
saveNewNote newDBNote con = do
  let message = getNewDBNoteText newDBNote
  execute con "INSERT INTO SCRIB (MESSAGE) VALUES (?)" (Only (message :: T.Text))
  let noteIdIO       = (pure . fromIntegral . toInteger <$> (lastInsertRowId con)) :: IO NoteId
      noteVersionIO  = (pure . pure $ minVersion) :: IO NoteVersion
  liftA2 NoteIdVersion  noteIdIO noteVersionIO

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
