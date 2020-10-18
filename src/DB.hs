{-# LANGUAGE OverloadedStrings #-}

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

import qualified Data.Text    as T

saveExitingNote :: DBNote -> Connection -> IO Int
saveExitingNote (DBNote id_ message_) con = do
    executeNamed con "UPDATE SCRIB SET MESSAGE = :message WHERE ID = :id" [":message" := message_,  ":id" := id_]
    pure id_

saveNewNote :: NewDBNote -> Connection -> IO Int
saveNewNote (NewDBNote message) con = do
     execute con "INSERT INTO SCRIB (MESSAGE) VALUES (?)" (Only (message :: T.Text))
     fromIntegral . toInteger <$> (lastInsertRowId con)

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
fetchNotes (FetchSize size) con = query con "SELECT ID, MESSAGE FROM SCRIB ORDER BY UPDATED_AT DESC LIMIT (?)" (Only size):: IO [DBNote]

searchNotes :: T.Text -> Connection -> IO [DBNote]
searchNotes searchCriteria con = query con "SELECT ID, MESSAGE FROM SCRIB WHERE MESSAGE LIKE (?)" (Only ("%" <> searchCriteria <> "%")) :: IO [DBNote]
