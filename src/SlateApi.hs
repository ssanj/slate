{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE DerivingStrategies  #-}
{-# LANGUAGE ScopedTypeVariables #-}

module SlateApi
       (
          -- Functions
         server
       , getIndexFile
       , getNotes2
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
setupScotty (SlateConfig apiKey dbConfig middlewareConfig errorHandler) =
  ST.scottyOptsT (serverOptions 3000) id $ do
    sequence_ (slateMiddleware middlewareConfig apiKey)

    sequence_ (slateErrorHandlers errorHandler)

    getIndexFile

    getNotes dbConfig

    performSearch dbConfig

    createNote dbConfig

    deleteNoteEndpoint dbConfig


slateMiddleware :: [MiddlewareType] -> ApiKey -> [SlateScottyAction]
slateMiddleware [] _                        = []
slateMiddleware (GZipping:rest) apiKey             = ST.middleware zipMiddleware                  : slateMiddleware rest apiKey
slateMiddleware (StaticFileServing:rest) apiKey    = ST.middleware staticFileMiddleware           : slateMiddleware rest apiKey
slateMiddleware (Logging:rest) apiKey              = ST.middleware loggingMiddleware              : slateMiddleware rest apiKey
slateMiddleware (ApiKeyRequiring:rest) apiKey      = ST.middleware (checkApiKeyMiddleware apiKey) : slateMiddleware rest apiKey

slateErrorHandlers :: Maybe SlateErrorHandler -> [SlateScottyAction]
slateErrorHandlers Nothing  = []
slateErrorHandlers (Just _) = pure $ ST.defaultHandler handleEx

createNote :: SlateDatabaseConfig -> SlateScottyAction
createNote dbConfig =
  ST.post "/note" $ do
    (note :: IncomingNote) <- jsonErrorHandle
    noteIdE <- withScribDb dbConfig (saveNote note)
    case noteIdE of
     (Left errorMessage) -> withError errorMessage
     (Right noteIdVersion) -> maybe (jsonResponse created201 noteIdVersion) (const $ jsonResponse ok200 noteIdVersion) (_incomingNoteAndVersion note)


performSearch :: SlateDatabaseConfig -> SlateScottyAction
performSearch dbConfig =
    ST.get "/search" $ do
      query <- ST.param "q"
      withScribDbActionM dbConfig (searchForNotes query) ST.json


getNotes :: SlateDatabaseConfig -> SlateScottyAction
getNotes dbConfig = ST.get "/notes" $ withScribDbActionM dbConfig retrieveTopNotes ST.json

getNotes2 :: Connection -> SlateScottyAction
getNotes2 con =
  let actionM :: SlateAction IO () = do
        value <- liftIO $ withTransaction con (putStrLn "before getNotes2" >> retrieveTopNotes con >> putStrLn "after getNotes2")
        ST.json value
  in ST.get "/notes" actionM

getIndexFile :: SlateScottyAction
getIndexFile = ST.get "/" $ ST.file "./static/index.html"

deleteNoteEndpoint :: SlateDatabaseConfig -> SlateScottyAction
deleteNoteEndpoint dbConfig =
  ST.delete "/note/:noteId" $ do
    noteId        <- mkNoteId <$> ST.param "noteId"
    withScribDb dbConfig (deleteNote noteId)
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

withError :: Monad m => DBError -> SlateAction m ()
withError dbError = ST.json (dbErrorToString dbError) >> ST.status status400

withScribDb :: MonadIO m => SlateDatabaseConfig -> (Connection -> IO a) -> SlateAction m a
withScribDb dbConfig = liftIO . scribDB dbConfig

withScribDbActionM :: MonadIO m => SlateDatabaseConfig -> (Connection -> IO a) -> (a -> SlateAction m b) -> SlateAction m b
withScribDbActionM dbConfig cb conversion = do
  value  <- liftIO $ scribDB dbConfig cb
  conversion value

scribDB :: SlateDatabaseConfig -> (Connection -> IO a) -> IO a
scribDB (SlateDatabaseConfig dbName) dbOp = withConnection (T.unpack dbName) (\con -> withTransaction con (dbOp con))

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
