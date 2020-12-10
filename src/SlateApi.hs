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

import Control.Monad.IO.Class       (liftIO, MonadIO)
import Data.Aeson                   (ToJSON(..))
import Database.SQLite.Simple       (Connection, withTransaction, withConnection)
import Network.HTTP.Types.Status    (Status, created201, ok200, status400)

import qualified Web.Scotty.Trans         as ST
import qualified Data.Text                as T

server :: ApiKey  -> IO ()
server apiKey =
  ST.scottyOptsT (serverOptions 3000) id $ do
    ST.middleware $ createMiddleware addStaticDirPolicy -- Need to have this first to serve static content
    ST.middleware $ checkApiKey apiKey
    ST.defaultHandler handleEx

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


withError :: Monad m => DBError -> SlateAction m ()
withError dbError = ST.json (dbErrorToString dbError) >> ST.status status400

withScribDb :: MonadIO m => (Connection -> IO a) -> SlateAction m a
withScribDb = liftIO . scribDB

withScribDbActionM :: MonadIO m => (Connection -> IO a) -> (a -> SlateAction m b) -> SlateAction m b
withScribDbActionM cb conversion = do
  value  <- liftIO $ scribDB cb
  conversion value

scribDB :: (Connection -> IO a) -> IO a
scribDB dbOp = withConnection "scrib.db" (\con -> withTransaction con (dbOp con))

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
