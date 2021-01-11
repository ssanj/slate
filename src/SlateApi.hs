{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE DerivingStrategies  #-}
{-# LANGUAGE ScopedTypeVariables #-}

module SlateApi
       (
          -- Functions
         server
       ) where

import Server
import DB
import Model
import Model.DBNote
import System.IO

import Control.Monad.IO.Class               (liftIO, MonadIO)
import Data.Aeson                           (ToJSON(..))
import Database.SQLite.Simple               (Connection, withTransaction, withConnection)
import Network.HTTP.Types.Status            (Status, created201, ok200, status400)
import Network.Wai.Middleware.RequestLogger (logStdout)
import Paths_slate                          (version)
import Control.Exception                    (catch, IOException)
import Control.DeepSeq                      (deepseq)

import qualified Network.Wai.Middleware.Gzip as GZ
import qualified Web.Scotty.Trans            as ST
import qualified Data.Text                   as T
import qualified Data.Text.IO                as T (putStrLn, putStr)
import qualified Data.Version                as DV

server :: ApiKey  -> IO ()
server apiKey = do
  printBanner
  setupScotty apiKey

setupScotty :: ApiKey -> IO ()
setupScotty apiKey =
  ST.scottyOptsT (serverOptions 3000) id $ do
    ST.middleware $ createMiddleware addStaticDirPolicy -- Need to have this first to serve static content
    ST.middleware $ GZ.gzip (GZ.def { GZ.gzipFiles = GZ.GzipCompress })
    ST.middleware logStdout
    ST.middleware $ checkApiKey apiKey
    ST.defaultHandler handleEx

    ST.get "/" $ ST.file "./static/index.html"

    ST.get "/notes" $ do
      withScribDbActionM retrieveTopNotes ST.json

    ST.get "/search" $ do
      query <- ST.param "q"
      withScribDbActionM (searchForNotes query) ST.json

    ST.post "/note" $ do
      (note :: IncomingNote) <- jsonErrorHandle
      noteIdE <- withScribDb (saveNote note)
      case noteIdE of
       (Left errorMessage) -> withError errorMessage
       (Right noteIdVersion) -> maybe (jsonResponse created201 noteIdVersion) (const $ jsonResponse ok200 noteIdVersion) (_incomingNoteAndVersion note)


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
  in either (pure . Left) (flip saveExitingNote $ con) dbNoteE
saveNote (IncomingNote noteText Nothing) con =
    let newDBNoteE = mkNewDBNote noteText
    in either (pure . Left) (\dbNote -> Right <$> (saveNewNote dbNote con)) newDBNoteE

searchForNotes :: T.Text -> Connection -> IO [OutgoingNote]
searchForNotes query con = fmap (fmap getOutgoingNote) (searchNotes query con)

retrieveTopNotes :: Connection -> IO [OutgoingNote]
retrieveTopNotes con = fmap (fmap getOutgoingNote) (fetchNotes maxFetchSize con)
