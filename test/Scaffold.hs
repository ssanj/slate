{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}

module Scaffold where

import Database.SQLite.Simple
import Database.SQLite.Simple.Time (parseUTCTime)
import Data.Tagged                 (Tagged(..), untag)
import DB.DBNote                   (DBNote, getDBNote, getNoteText, NoteId, NoteText, NoteVersion)
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


assertDBNote :: DBNote -> (Text -> IO ()) -> IO ()
assertDBNote dbNote assertion =
  let (_, dbMessage, _) = getDBNote dbNote
  in assertion $ getNoteText dbMessage


testDatabaseName :: DBName
testDatabaseName = Tagged ":memory:"


getDBName :: DBName -> String
getDBName = untag


dbTestBase :: DBName -> DBTest a -> (DBTest a -> Connection -> IO a) -> IO a
dbTestBase dbName dbt dbRunner =
  bracket
    (open (getDBName dbName))
    close
    (dbRunner dbt)


dbTest :: DBName -> DBTest a -> IO a
dbTest dbName dbt = dbTestBase dbName dbt runDatabaseChanges


dbTestTx :: DBName -> DBTest a -> IO a
dbTestTx dbName dbt = dbTestBase dbName dbt runDatabaseChangesTx


dbTest_ :: DBName -> DBTest () -> IO ()
dbTest_ dbn dbt = void $ dbTest dbn dbt


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


insertSpecificMessage :: Int -> Text ->DBAction ()
insertSpecificMessage id_ message = \con ->
  execute con "INSERT INTO SCRIB(ID, MESSAGE) VALUES (?,?)" (id_, message)


insertMessageNumbered :: Int -> DBAction ()
insertMessageNumbered item = \con ->
  let message = "# Test message " <> (pack . show $ item) <> "\n This is only a test" :: Text
  in insertMessage (message, "2010-05-28T15:36:56.200", False) con

findDBNote :: Int -> DBAction (Maybe (NoteId, NoteText, NoteVersion))
findDBNote dbNoteId = \con -> do
   dbNotes <- query con "SELECT ID, MESSAGE, VERSION FROM SCRIB WHERE ID = ?" (Only dbNoteId):: IO [DBNote]
   case dbNotes of
    []         -> pure Nothing
    (dbNote:_) -> pure . Just . getDBNote $ dbNote

deleteSchema :: CleanUp -> DBAction ()
deleteSchema _ con = execute_ con "DROP TABLE IF EXISTS SCRIB"


runAssertionFailure :: String -> IO ((), CleanUp)
runAssertionFailure = assertFailure


runAssertionSuccess :: IO ((), CleanUp)
runAssertionSuccess = pure ((), AssertionRun)


dbNoteTest :: (InitialisedDB -> DBAction ((), SeededDB)) -> (SeededDB -> DBAction ((), CleanUp)) -> Assertion
dbNoteTest seedF assertF = dbTest_ testDatabaseName $ DBTest createSchema seedF assertF deleteSchema


dbWithinTxTest :: (InitialisedDB -> DBAction ((), SeededDB)) -> (SeededDB -> DBAction (a, CleanUp)) -> IO a
dbWithinTxTest seedF action = dbTestTx testDatabaseName $ DBTest createSchema seedF action deleteSchema


runAssertion :: IO a -> IO (a, CleanUp)
runAssertion assertion = ((, AssertionRun)) <$> assertion

noTestData :: InitialisedDB -> DBAction ((), SeededDB)
noTestData _ = \_ ->  pure ((), SeededDB)
