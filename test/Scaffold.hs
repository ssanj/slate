{-# LANGUAGE OverloadedStrings #-}

module Scaffold where

import Database.SQLite.Simple
import Database.SQLite.Simple.Time (parseUTCTime)
import Data.Tagged                 (Tagged(..), untag)
import DB.DBNote                   (DBNote, getDBNote, getNoteText)
import Data.Text                   (Text, pack)
import Control.Exception           (bracket)
import Control.Monad               (void)
import Test.Tasty.HUnit            (assertFailure, Assertion)

data InitialisedTag
data SeededTag

type DBAction a = Connection -> IO a

data DBNameTag

type DBName = Tagged DBNameTag String

data InitialisedDB = InitialisedDB
data SeededDB = SeededDB

data CleanUp = AssertionRun
             | Initialise

data DBTest a =
  DBTest {
    init     :: DBAction ((), InitialisedDB)
  , seedData :: InitialisedDB -> DBAction ((), SeededDB)
  , dbSpec :: SeededDB -> DBAction (a, CleanUp)
  , cleanUp  :: CleanUp -> DBAction ()
  }


-- data DBTestWithoutSpec =
--   DBTestWithoutSpec {
--     dbTestWithoutSpecInit     :: DBAction ((), InitialisedDB)
--   , dbTestWithoutSpecSeedData :: InitialisedDB -> DBAction ((), SeededDB)
--   , dbTestWithoutSpecCleanUp  :: CleanUp -> DBAction ()
--   }

-- toDbTest :: DBTestWithoutSpec -> DBAction a -> DBTest
-- toDbTest (DBTestWithoutSpec init' seed' cleanup') action =
--   let dbSpec' :: SeededDB -> DBAction ((), CleanUp)
--       dbSpec' _ = \c -> fmap (const ((), AssertionRun)) (action c)
--   in
--     DBTest
--       init'
--       seed'
--       dbSpec'
--       cleanup'

assertDBNote :: DBNote -> (Text -> IO ()) -> IO ()
assertDBNote dbNote assertion =
  let (_, dbMessage, _) = getDBNote dbNote
  in assertion $ getNoteText dbMessage

testDatabaseName :: DBName
testDatabaseName = Tagged ":memory:"

getDBName :: DBName -> String
getDBName = untag

dbTest :: DBName -> DBTest a -> IO a
dbTest dbName dbt =
  bracket
    (open (getDBName dbName))
    close
    (runDatabaseChanges dbt)

dbTestTx :: DBName -> DBTest a -> IO a
dbTestTx dbName dbt =
  bracket
    (open (getDBName dbName))
    close
    (runDatabaseChangesTx dbt)

dbTest_ :: DBName -> DBTest () -> IO ()
dbTest_ dbn dbt = void $ dbTest dbn dbt

-- dbTest' :: DBName -> DBTestWithoutSpec -> DBAction a -> IO ()
-- dbTest' dbName dbtws assertSpec =
--   bracket
--     (open (getDBName dbName))
--     close
--     (runDatabaseChanges (toDbTest dbtws assertSpec))

runDatabaseChanges :: DBTest a -> Connection -> IO a
runDatabaseChanges (DBTest runInit runSeedData assertWith runCleanUp) con =
  withTransaction con $ do
    runCleanUp Initialise con
    (_, token1)      <- runInit con
    (_, token2)      <- runSeedData token1 con
    (result, token3) <- assertWith token2 con
    runCleanUp token3 con
    pure result

-- because sqlite does not support nested transaction, if your code under test needs a transaction it will
-- fail with an error. This version of `runDatabaseChanges` runs each of (cleanUp + init), `seedData` and
-- `cleanUp` in separate transaction, allowing you to run your code under test in its own transaction
runDatabaseChangesTx :: DBTest a -> Connection -> IO a
runDatabaseChangesTx (DBTest runInit runSeedData assertWith runCleanUp) con =
  do
    (_, token1)      <- withTransaction con $ (runCleanUp Initialise con >> runInit con)
    (_, token2)      <- withTransaction con $ runSeedData token1 con
    (result, token3) <- assertWith token2 con
    withTransaction con $ runCleanUp token3 con
    pure result


createSchema :: DBAction ((), InitialisedDB)
createSchema con =
  execute_
    con
    "CREATE TABLE IF NOT EXISTS SCRIB( \
      \ ID INTEGER PRIMARY KEY AUTOINCREMENT, \
      \ MESSAGE TEXT NOT NULL, \
      \ CREATED_AT TIMESTAMP DEFAULT CURRENT_TIMESTAMP NOT NULL, \
      \ UPDATED_AT TIMESTAMP DEFAULT CURRENT_TIMESTAMP NOT NULL, \
      \ VERSION INT DEFAULT 1 NOT NULL, \
      \ DELETED INTEGER NOT NULL DEFAULT 0 CHECK (DELETED IN (0,1)) \
      \ );"
     >> (pure $ ((), InitialisedDB))

insertMessage :: (Text, Text, Bool) -> DBAction ()
insertMessage (message, date, deleted) = \con ->
  let dateTimeE = parseUTCTime date
  in
    case dateTimeE of
      Left x         -> ioError . userError $ x
      Right dateTime -> execute con "INSERT INTO SCRIB(MESSAGE, CREATED_AT, UPDATED_AT, DELETED) VALUES (?,?,?,?)" (message, dateTime, dateTime, deleted)

insertMessageNumbered :: Int -> DBAction ()
insertMessageNumbered item = \con ->
  let message = "# Test message " <> (pack . show $ item) <> "\n This is only a test" :: Text
  in insertMessage (message, "2010-05-28T15:36:56.200", False) con


deleteSchema :: CleanUp -> DBAction ()
deleteSchema _ con = execute_ con "DROP TABLE IF EXISTS SCRIB"

runAssertion :: IO a -> IO (a, CleanUp)
runAssertion action = (\a -> (a, AssertionRun)) <$> action

runAssertionFailure :: String -> IO ((), CleanUp)
runAssertionFailure = assertFailure

runAssertionSuccess :: IO ((), CleanUp)
runAssertionSuccess = pure ((), AssertionRun)

dbNoteTest :: (InitialisedDB -> DBAction ((), SeededDB)) -> (SeededDB -> DBAction ((), CleanUp)) -> Assertion
dbNoteTest seedF assertF = dbTest_ testDatabaseName $ DBTest createSchema seedF assertF deleteSchema

dbWithinTxTest :: (InitialisedDB -> DBAction ((), SeededDB)) -> (SeededDB -> DBAction (a, CleanUp)) -> IO a
dbWithinTxTest seedF action = dbTestTx testDatabaseName $ DBTest createSchema seedF action deleteSchema
