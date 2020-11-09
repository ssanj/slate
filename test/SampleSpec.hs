{-# LANGUAGE OverloadedStrings #-}

module SampleSpec where

-- import qualified Hedgehog              as H
-- import qualified Hedgehog.Gen          as Gen
-- import qualified Hedgehog.Range        as Range

import Database.SQLite.Simple
import Database.SQLite.Simple.Time (parseUTCTime)
import Test.Tasty.HUnit ((@?=), Assertion, assertFailure)
import Data.Tagged       (Tagged(..), untag)
import Data.Foldable    (traverse_)
import DB               (fetchNotes, fetchSize)
import Model.DBNote     (DBNote, getDBNote, getNoteText)
import Data.Text        (Text, pack)
import Control.Exception (bracket)

-- hprop_equality :: H.Property
-- hprop_equality =
--   H.property $ do
--     randomString <- H.forAll $ Gen.text (Range.linear 0 100) Gen.alpha
--     randomString H.=== randomString

unit_fetchNotes :: Assertion
unit_fetchNotes = dbTest testDatabaseName $ DBTest createSchema insertSeedData assert_fetchNotes deleteSchema

assert_fetchNotes :: SeededDB -> DBAction ((), AssertionRun)
assert_fetchNotes _ = \con -> do
  notes <- fetchNotes (fetchSize 1) con
  case notes of
    []       -> (\_ -> ((), AssertionRun)) <$> assertFailure "Expected to find a note"
    (note:_) -> (\_ -> ((), AssertionRun)) <$> assertDBNote note "# Another note\nMore and more"

assertDBNote :: DBNote -> Text -> IO ()
assertDBNote dbNote message =
  let (_, dbMessage, _) = getDBNote dbNote
  in getNoteText dbMessage @?= message

testDatabaseName :: DBName
testDatabaseName = Tagged "test.db"

data InitialisedTag
data SeededTag

type DBAction a = Connection -> IO a

data DBNameTag

type DBName = Tagged DBNameTag String

data InitialisedDB = InitialisedDB
data SeededDB = SeededDB
data AssertionRun = AssertionRun

getDBName :: DBName -> String
getDBName = untag

data DBTest =
  DBTest {
    init     :: DBAction ((), InitialisedDB)
  , seedData :: InitialisedDB -> DBAction ((), SeededDB)
  , dbSpec :: SeededDB -> DBAction ((), AssertionRun)
  , cleanUp  :: AssertionRun -> DBAction ()
  }

dbTest :: DBName -> DBTest -> IO ()
dbTest dbName dbt =
  bracket
    (open (getDBName dbName))
    close
    (runDatabaseChanges dbt)

runDatabaseChanges :: DBTest -> Connection -> IO ()
runDatabaseChanges (DBTest runInit runSeedData assertWith runCleanUp) con =
  withTransaction con $ do
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

insertSeedData :: InitialisedDB -> DBAction ((), SeededDB)
insertSeedData _ = \con -> do
  traverse_ (\n -> insertMessageNumbered n con) [1..20]
  traverse_
    (\msg -> insertMessage msg con)
    [
      ("# Some Note\nYolo", "2020-06-01T15:36:56.200")
    , ("# Another note\nMore and more", "2020-06-09T15:36:56.200")
    , ("# Blog Article\nThis is a blog article about..", "2020-06-02T15:36:56.200")
    ]
  pure ((), SeededDB)

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


deleteSchema :: AssertionRun -> DBAction ()
deleteSchema _ con= execute_ con "DROP TABLE IF EXISTS SCRIB"
