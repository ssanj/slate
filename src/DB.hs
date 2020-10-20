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
saveExitingNote (DBNote id_ message_ version_) con = do
    -- let versions = [1] :: [Int]
    versions <-  query con "SELECT VERSION FROM SCRIB WHERE ID = ?" (Only (id_ :: Int)) :: IO [Only Int]
       -- queryNamed con "SELECT VERSION FROM SCRIB WHERE ID = :id" [":id" := id_]
    case versions of
      []    -> pure . Left $ ItemNotFound id_
      ((Only oldVersion):_) ->
        let validVersionRange   = version_ >= minVersion && version_ < maxVersion -- one less than max to allow for one final increment
            sameVersionAsClient = oldVersion == version_
        in
          case (validVersionRange, sameVersionAsClient) of
            (True, True) ->
              do
                let newVersion = version_ + 1
                executeNamed con
                  "UPDATE SCRIB SET MESSAGE = :message, VERSION = :newVersion WHERE ID = :id and VERSION = :oldVersion"
                    [
                      ":message" := message_
                    , ":id" := id_
                    , ":oldVersion" := oldVersion
                    , ":newVersion" := newVersion
                    ]
                pure . Right $ NoteIdVersion (pure id_) (pure newVersion)
            (False, _) -> pure . Left $ InvalidVersion version_
            (_, False) -> pure . Left $ VersionMismatch oldVersion version_

saveNewNote :: NewDBNote -> Connection -> IO NoteIdVersion
saveNewNote (NewDBNote message) con = do
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
fetchNotes (FetchSize size) con = query con "SELECT ID, MESSAGE, VERSION FROM SCRIB ORDER BY UPDATED_AT DESC LIMIT (?)" (Only size):: IO [DBNote]

searchNotes :: T.Text -> Connection -> IO [DBNote]
searchNotes searchCriteria con = query con "SELECT ID, MESSAGE, VERSION FROM SCRIB WHERE MESSAGE LIKE (?)" (Only ("%" <> searchCriteria <> "%")) :: IO [DBNote]
