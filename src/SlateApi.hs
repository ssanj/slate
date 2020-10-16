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
import Database.SQLite.Simple (Connection, withConnection)
import Network.HTTP.Types.Status (created201, ok200)

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
      noteId <- withScribDb (saveNote note)
      maybe (created noteId) ok (_incomingNoteId note)

withScribDb :: (Connection -> IO a) -> ActionM a
withScribDb = liftIO . scribDB

withScribDbActionM :: (Connection -> IO a) -> (a -> ActionM b) -> ActionM b
withScribDbActionM cb conversion = do
  value  <- liftIO $ scribDB cb
  conversion value

scribDB :: (Connection -> IO a) -> IO a
scribDB = withConnection "scrib.db"

ok :: ToJSON a => a -> ActionM ()
ok value = SC.json value  >>  status ok200

created :: Int -> ActionM ()
created noteId = SC.json (noteId :: Int) >> status created201

saveNote :: IncomingNote -> Connection -> IO Int
saveNote (IncomingNote noteText (Just noteId)) = saveExitingNote (DBNote noteId noteText)
saveNote (IncomingNote noteText Nothing)       = saveNewNote (NewDBNote noteText)

searchForNotes :: T.Text -> Connection -> IO [OutgoingNote]
searchForNotes query con = fmap (fmap createNote) (searchNotes query con)

retrieveTopNotes :: Connection -> IO [OutgoingNote]
retrieveTopNotes con = fmap (fmap createNote) (fetchNotes maxFetchSize con)

createNote :: DBNote -> OutgoingNote
createNote (DBNote noteId noteText) = OutgoingNote noteText noteId

