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
import Web.Scotty         hiding    (json, Options)

import Control.Monad.IO.Class       (liftIO)
import Data.Aeson                   (ToJSON)
import Database.SQLite.Simple       (Connection, withTransaction, withConnection)
import Network.HTTP.Types.Status    (Status, created201, ok200, status400)
import Data.Tagged                  (untag)

import qualified Web.Scotty   as SC (json)
import qualified Data.Text    as T

server :: ApiKey  -> IO ()
server apiKey =
  scotty 3000 $ do
    middleware $ createMiddleware addStaticDirPolicy -- Need to have this first to serve static content
    middleware $ checkApiKey apiKey
    get "/notes" $ do
      withScribDbActionM retrieveTopNotes SC.json

    get "/search" $ do
      query <- param "q"
      withScribDbActionM (searchForNotes query) SC.json

    post "/note" $ do
      (note :: IncomingNote) <- jsonData
      noteIdE <- withScribDb (saveNote note)
      case noteIdE of
       (Left errorMessage) -> withError errorMessage
       (Right noteIdVersion) -> maybe (jsonResponse created201 noteIdVersion) (const $ jsonResponse ok200 noteIdVersion) (_incomingNoteId note)


withError :: DBError -> ActionM ()
withError dbError = SC.json (dbErrorToString dbError) >> status status400

withScribDb :: (Connection -> IO a) -> ActionM a
withScribDb = liftIO . scribDB

withScribDbActionM :: (Connection -> IO a) -> (a -> ActionM b) -> ActionM b
withScribDbActionM cb conversion = do
  value  <- liftIO $ scribDB cb
  conversion value

scribDB :: (Connection -> IO a) -> IO a
scribDB dbOp = withConnection "scrib.db" (\con -> withTransaction con (dbOp con))

jsonResponse :: ToJSON a => Status -> a -> ActionM ()
jsonResponse st value = SC.json value >> status st

saveNote :: IncomingNote -> Connection -> IO (Either DBError NoteIdVersion)
saveNote (IncomingNote noteText (Just noteId) (Just version)) con =
  case createDBNote (mkNoteId noteId) noteText (mkNoteVersion version) of
    Left x       -> pure $ Left x
    Right dbNote -> saveExitingNote dbNote con

saveNote (IncomingNote noteText Nothing Nothing) con =
    pure <$> (saveNewNote (NewDBNote noteText) con)
saveNote _ _ = pure . Left $ NeedIdAndVersion

searchForNotes :: T.Text -> Connection -> IO [OutgoingNote]
searchForNotes query con = fmap (fmap createNote) (searchNotes query con)

retrieveTopNotes :: Connection -> IO [OutgoingNote]
retrieveTopNotes con = fmap (fmap createNote) (fetchNotes maxFetchSize con)

createNote :: DBNote -> OutgoingNote
createNote dbNote =
  let (noteId, noteText, noteVersion) = getDBNote dbNote
  in OutgoingNote (getNoteText noteText) (untag noteId) (untag noteVersion)