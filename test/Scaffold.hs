{-# LANGUAGE OverloadedStrings #-}

module Scaffold where

import Database.SQLite.Simple
import Database.SQLite.Simple.Time (parseUTCTime)
import Data.Tagged       (Tagged(..), untag)
import Model.DBNote     (DBNote, getDBNote, getNoteText)
import Data.Text        (Text, pack)
import Control.Exception (bracket)
import Test.Tasty.HUnit (assertFailure)

data InitialisedTag
data SeededTag

type DBAction a = Connection -> IO a

data DBNameTag

type DBName = Tagged DBNameTag String

data InitialisedDB = InitialisedDB
data SeededDB = SeededDB

data CleanUp = AssertionRun
             | Initialise

data DBTest =
  DBTest {
    init     :: DBAction ((), InitialisedDB)
  , seedData :: InitialisedDB -> DBAction ((), SeededDB)
  , dbSpec :: SeededDB -> DBAction ((), CleanUp)
  , cleanUp  :: CleanUp -> DBAction ()
  }

assertDBNote :: DBNote -> (Text -> IO ()) -> IO ()
assertDBNote dbNote assertion =
  let (_, dbMessage, _) = getDBNote dbNote
  in assertion $ getNoteText dbMessage

testDatabaseName :: DBName
testDatabaseName = Tagged "test.db"

getDBName :: DBName -> String
getDBName = untag

dbTest :: DBName -> DBTest -> IO ()
dbTest dbName dbt =
  bracket
    (open (getDBName dbName))
    close
    (runDatabaseChanges dbt)

runDatabaseChanges :: DBTest -> Connection -> IO ()
runDatabaseChanges (DBTest runInit runSeedData assertWith runCleanUp) con =
  withTransaction con $ do
    runCleanUp Initialise con
    (_, token1) <- runInit con
    (_, token2) <- runSeedData token1 con
    (_, token3) <- assertWith token2 con
    runCleanUp token3 con

createSchema :: DBAction ((), InitialisedDB)
createSchema con =
  execute_
    con
    "CREATE TABLE IF NOT EXISTS SCRIB( \
      \ ID INTEGER PRIMARY KEY AUTOINCREMENT, \
      \ MESSAGE TEXT NOT NULL, \
      \ CREATED_AT TIMESTAMP DEFAULT CURRENT_TIMESTAMP NOT NULL, \
      \ UPDATED_AT TIMESTAMP DEFAULT CURRENT_TIMESTAMP NOT NULL, \
      \ VERSION INT DEFAULT 1 NOT NULL \
      \ );"
     >> (pure $ ((), InitialisedDB))

insertMessage :: (Text, Text) -> DBAction ()
insertMessage (message, date) = \con ->
  let dateTimeE = parseUTCTime date
  in
    case dateTimeE of
      Left x         -> ioError . userError $ x
      Right dateTime -> execute con "INSERT INTO SCRIB(MESSAGE, CREATED_AT, UPDATED_AT) VALUES (?,?,?)" (message, dateTime, dateTime)

insertMessageNumbered :: Int -> DBAction ()
insertMessageNumbered item = \con ->
  let message = "# Test message " <> (pack . show $ item) <> "\n This is only a test" :: Text
  in insertMessage (message, "2010-05-28T15:36:56.200") con


deleteSchema :: CleanUp -> DBAction ()
deleteSchema _ con= execute_ con "DROP TABLE IF EXISTS SCRIB"

runAssertion :: IO a -> IO (a, CleanUp)
runAssertion action = (\a -> (a, AssertionRun)) <$> action

runAssertionFailure :: String -> IO ((), CleanUp)
runAssertionFailure = assertFailure

runAssertionSuccess :: IO ((), CleanUp)
runAssertionSuccess = pure ((), AssertionRun)
