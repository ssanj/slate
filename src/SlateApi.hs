{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE DerivingStrategies  #-}
{-# LANGUAGE ScopedTypeVariables #-}

module SlateApi
       (
          -- Functions
         server
       , getIndexFile
       ) where

import Server
import DB.Repository
import Model
import DB.DBNote
import System.IO

import Control.Monad.IO.Class               (liftIO, MonadIO)
import Data.Aeson                           (ToJSON(..))
import Database.SQLite.Simple               (Connection, withTransaction, withConnection)
import Network.HTTP.Types.Status            (Status, created201, ok200, status400, noContent204)

import Paths_slate                          (version)
import Control.Exception                    (catch, IOException)
import Control.DeepSeq                      (deepseq)

import qualified Web.Scotty.Trans            as ST
import qualified Data.Text                   as T
import qualified Data.Text.IO                as T (putStrLn, putStr)
import qualified Data.Version                as DV

server :: SlateConfig  -> IO ()
server slateConfig = do
  printBanner
  setupScotty slateConfig

setupScotty :: SlateConfig -> IO ()
setupScotty slateConfig =
  ST.scottyOptsT (serverOptions 3000) id $ do
    sequence_ . slateMiddleware . _slateConfigApiKey $ slateConfig

    ST.defaultHandler handleEx

    getIndexFile

    getNotes

    performSearch

    createNote

    deleteNoteEndpoint


slateMiddleware :: ApiKey -> [SlateScottyAction]
slateMiddleware apiKey =
  [
    ST.middleware zipMiddleware
  , ST.middleware loggingMiddleware
  , ST.middleware staticFileMiddleware
  , ST.middleware $ checkApiKeyMiddleware apiKey
  ]

createNote :: SlateScottyAction
createNote =
  ST.post "/note" $ do
    (note :: IncomingNote) <- jsonErrorHandle
    noteIdE <- withScribDb (saveNote note)
    case noteIdE of
     (Left errorMessage) -> withError errorMessage
     (Right noteIdVersion) -> maybe (jsonResponse created201 noteIdVersion) (const $ jsonResponse ok200 noteIdVersion) (_incomingNoteAndVersion note)


performSearch :: SlateScottyAction
performSearch =
    ST.get "/search" $ do
      query <- ST.param "q"
      withScribDbActionM (searchForNotes query) ST.json


getNotes :: SlateScottyAction
getNotes = ST.get "/notes" $ withScribDbActionM retrieveTopNotes ST.json

getIndexFile :: SlateScottyAction
getIndexFile = ST.get "/" $ ST.file "./static/index.html"

deleteNoteEndpoint ::SlateScottyAction
deleteNoteEndpoint =
  ST.delete "/note/:noteId" $ do
    noteId        <- mkNoteId <$> ST.param "noteId"
    withScribDb (deleteNote noteId)
    ST.status noContent204

printBanner :: IO ()
printBanner = do
  getAsciiBanner >>= T.putStrLn
  T.putStr . T.pack . take 17 . repeat $ ' '
  T.putStr . T.pack . DV.showVersion $ version
  T.putStrLn ""
  T.putStrLn ""

getAsciiBanner :: IO T.Text
getAsciiBanner =
  catch (withFile "banner.txt" ReadMode readBannerContent) handleBannerNotFound
    where
      readBannerContent :: Handle -> IO T.Text
      readBannerContent h = do
        hSetBuffering h NoBuffering
        content <- hGetContents h
        content `deepseq` (pure . T.pack $ content)

      handleBannerNotFound :: IOException -> IO T.Text
      handleBannerNotFound _ = do
        hPutStr stderr "Could not open banner.txt\n"
        pure "= SLATE ="


databaseLocation :: T.Text
databaseLocation = "db"

withDatabaseLocation :: T.Text -> T.Text
withDatabaseLocation dbName = databaseLocation <> "/" <> dbName

withError :: Monad m => DBError -> SlateAction m ()
withError dbError = ST.json (dbErrorToString dbError) >> ST.status status400

withScribDb :: MonadIO m => (Connection -> IO a) -> SlateAction m a
withScribDb = liftIO . scribDB

withScribDbActionM :: MonadIO m => (Connection -> IO a) -> (a -> SlateAction m b) -> SlateAction m b
withScribDbActionM cb conversion = do
  value  <- liftIO $ scribDB cb
  conversion value

scribDB :: (Connection -> IO a) -> IO a
scribDB dbOp = withConnection (T.unpack $ withDatabaseLocation "scrib.db") (\con -> withTransaction con (dbOp con))

jsonResponse :: (ToJSON a, Monad m) => Status -> a -> SlateAction m ()
jsonResponse st value = ST.json value >> ST.status st

saveNote :: IncomingNote -> Connection -> IO (Either DBError NoteIdVersion)
saveNote (IncomingNote noteText (Just noteIdAndVersion)) con =
  let (noteId, noteVersion) = getNoteIdAndNoteVersion noteIdAndVersion
      dbNoteE = createDBNote noteId noteText noteVersion
  in either (pure . Left) (flip saveExistingNote $ con) dbNoteE
saveNote (IncomingNote noteText Nothing) con =
    let newDBNoteE = mkNewDBNote noteText
    in either (pure . Left) (\dbNote -> Right <$> (saveNewNote dbNote con)) newDBNoteE

searchForNotes :: T.Text -> Connection -> IO [OutgoingNote]
searchForNotes query con = fmap (fmap getOutgoingNote) (searchNotes query con)

retrieveTopNotes :: Connection -> IO [OutgoingNote]
retrieveTopNotes con = fmap (fmap getOutgoingNote) (fetchNotes maxFetchSize con)

deleteNote :: NoteId -> Connection -> IO ()
deleteNote = deactivateNote
