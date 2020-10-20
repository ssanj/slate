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

import Control.Monad.IO.Class (liftIO)
import Web.Scotty hiding (json, Options)
import Data.Aeson (ToJSON)
import Database.SQLite.Simple (Connection, withTransaction, withConnection)
import Network.HTTP.Types.Status (Status, created201, ok200, status400)

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

-- TODO: Prob send back a Json object with ErrorId and Message
dbErrorToString :: DBError -> OutgoingError
dbErrorToString db@(ItemNotFound _)      = OutgoingError (getDBErrorCode db) "The note specified could not be found"
dbErrorToString db@(InvalidVersion _)    = OutgoingError (getDBErrorCode db) "The version of the note supplied is invalid"
dbErrorToString db@NeedIdAndVersion      = OutgoingError (getDBErrorCode db) "The save did not send the expected information to the server"
dbErrorToString db@(VersionMismatch _ _) = OutgoingError (getDBErrorCode db) "There's a different version of this note on the server. Refresh and try again"

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
  saveExitingNote (DBNote noteId noteText version) con
saveNote (IncomingNote noteText Nothing Nothing) con =
   pure <$> (saveNewNote (NewDBNote noteText) con)
saveNote _ _ = pure . Left $ NeedIdAndVersion

searchForNotes :: T.Text -> Connection -> IO [OutgoingNote]
searchForNotes query con = fmap (fmap createNote) (searchNotes query con)

retrieveTopNotes :: Connection -> IO [OutgoingNote]
retrieveTopNotes con = fmap (fmap createNote) (fetchNotes maxFetchSize con)

createNote :: DBNote -> OutgoingNote
createNote (DBNote noteId noteText noteVersion) = OutgoingNote noteText noteId noteVersion