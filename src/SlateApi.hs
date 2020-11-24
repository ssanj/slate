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
-- import Web.Scotty         hiding    (json, Options)
import Web.Scotty.Trans             (ActionT, ScottyError(..))
import Network.HTTP.Types           (status422)
import Data.String                  (fromString)

import Control.Monad                (when)
import Control.Monad.IO.Class       (liftIO)
import Data.Aeson                   (ToJSON(..), FromJSON(..), eitherDecode, Result(..), fromJSON)
import Database.SQLite.Simple       (Connection, withTransaction, withConnection)
import Network.HTTP.Types.Status    (Status, created201, ok200, status400, status500)

import Web.Scotty.Trans               as ST (json, body, scottyT, middleware, defaultHandler, get, param, post, status, raise)
import qualified Data.Text            as T
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text.Encoding   as E

type SlateAction = ActionT Except IO

server :: ApiKey  -> IO ()
server apiKey =
  ST.scottyT 3000 id $ do
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


withError :: DBError -> SlateAction ()
withError dbError = ST.json (dbErrorToString dbError) >> ST.status status400

withScribDb :: (Connection -> IO a) -> SlateAction a
withScribDb = liftIO . scribDB

withScribDbActionM :: (Connection -> IO a) -> (a -> SlateAction b) -> SlateAction b
withScribDbActionM cb conversion = do
  value  <- liftIO $ scribDB cb
  conversion value

scribDB :: (Connection -> IO a) -> IO a
scribDB dbOp = withConnection "scrib.db" (\con -> withTransaction con (dbOp con))

jsonResponse :: ToJSON a => Status -> a -> SlateAction ()
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

-- MOVE TO SCOTTY INTERNAL MODULE

data Except = MalformedJsonInput T.Text
            | InvalidInput T.Text
            | NoDataProvided T.Text
            | GenericError T.Text
    deriving stock (Show, Eq)


instance ScottyError Except where
    stringError = GenericError . T.pack
    showError = fromString . show


handleEx :: Except -> SlateAction ()
handleEx (MalformedJsonInput errorText) = ST.status status400 >> ST.json (OutgoingError 900 errorText)
handleEx (NoDataProvided errorText)     = ST.status status400 >> ST.json (OutgoingError 901 errorText)
handleEx (InvalidInput errorText)       = ST.status status422 >> ST.json (OutgoingError 902 errorText)
handleEx (GenericError errorText)       = ST.status status500 >> ST.json (OutgoingError 903 errorText)

jsonErrorHandle :: FromJSON a => SlateAction a
jsonErrorHandle = do
    b <- ST.body
    when (b == "") $ do
      let errorMessage = "jsonData - No data was provided." :: T.Text
      ST.raise $ NoDataProvided errorMessage
    case eitherDecode b of
      Left err -> do
        let errorMessage =
              "jsonData - malformed." <>
              " Data was: "           <>
              toText b                <>
              " Error was: "          <>
              T.pack err
        ST.raise $ MalformedJsonInput errorMessage

      Right value -> case fromJSON value of

        Error err -> do
          let errorMessage =
                "jsonData - failed parse." <>
                " Data was: "              <>
                toText b                   <>
                "."                        <>
                " Error was: "             <>
                T.pack err
          ST.raise $ InvalidInput errorMessage

        Success a -> do
          return a

toText :: BL.ByteString -> T.Text
toText = E.decodeUtf8 . BL.toStrict
