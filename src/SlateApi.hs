{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE DerivingStrategies  #-}
{-# LANGUAGE ScopedTypeVariables #-}

module SlateApi
       (
          -- Functions
         server
       , getIndexFile
       , getNotesEndpoint
       , performSearchEndpoint
       , createNoteEndpoint
       , deleteNoteEndpoint
       , slateMiddleware
       ) where

import Server
import DB.Repository
import Model
import DB.DBNote
import System.IO

import Control.Monad.IO.Class               (liftIO)
import Data.Aeson                           (ToJSON(..))
import Database.SQLite.Simple               (Connection, withTransaction)

import Network.HTTP.Types.Status            (Status, created201, ok200, status400, noContent204)

import Paths_slate                          (version)
import Control.Exception                    (IOException, catch, bracket)
import Control.DeepSeq                      (deepseq)
import Data.Time.Clock                      (NominalDiffTime)

import qualified Web.Scotty.Trans            as ST
import qualified Data.Text                   as T
import qualified Data.Text.IO                as T (putStrLn, putStr)
import qualified Data.Version                as DV
import qualified Data.Pool                   as P
import qualified Database.SQLite.Simple      as SQLITE

server :: SlateConfig  -> IO ()
server slateConfig = do
  printBanner
  bracket
    (createConnectionPool (_slateConfigDatabaseConfig slateConfig))
     destroyConnectionPool
     (\pool -> withPooledConnection pool (setupScotty slateConfig))


destroyConnectionPool :: P.Pool Connection -> IO ()
destroyConnectionPool pool = do
  putStrLn "Cleaning ze pool on shutdown"
  P.destroyAllResources pool


createConnectionPool :: SlateDatabaseConfig -> IO (P.Pool Connection)
createConnectionPool dbConfig =
  let  stripes :: Int                        = 1
       idleTimeInSeconds :: NominalDiffTime  = 10
       connections :: Int                    = 5
  in P.createPool
      (SQLITE.open . T.unpack ._slateDatabaseConfigDatabaseLocation $ dbConfig)
      SQLITE.close
      stripes
      idleTimeInSeconds
      connections


setupScotty :: SlateConfig -> Connection -> IO ()
setupScotty (SlateConfig apiKey _ middlewareConfig errorHandler staticDir) con =
  ST.scottyOptsT (serverOptions 3000) id $ do
    sequence_ (slateMiddleware middlewareConfig apiKey staticDir)

    sequence_ (slateErrorHandlers errorHandler)

    getIndexFile staticDir

    databaseActions con


databaseActions :: Connection -> SlateScottyAction
databaseActions con = do
    getNotesEndpoint      con
    performSearchEndpoint con
    createNoteEndpoint    con
    deleteNoteEndpoint    con


slateMiddleware :: [MiddlewareType] -> ApiKey -> StaticFileDir -> [SlateScottyAction]
slateMiddleware [] _  _                               = []
slateMiddleware (GZipping:rest) apiKey sfDir          = ST.middleware zipMiddleware                  : slateMiddleware rest apiKey sfDir
slateMiddleware (StaticFileServing:rest) apiKey sfDir = ST.middleware (staticFileMiddleware sfDir)   : slateMiddleware rest apiKey sfDir
slateMiddleware (Logging:rest) apiKey sfDir           = ST.middleware loggingMiddleware              : slateMiddleware rest apiKey sfDir
slateMiddleware (ApiKeyRequiring:rest) apiKey sfDir   = ST.middleware (checkApiKeyMiddleware apiKey) : slateMiddleware rest apiKey sfDir


slateErrorHandlers :: Maybe SlateErrorHandler -> [SlateScottyAction]
slateErrorHandlers Nothing  = []
slateErrorHandlers (Just _) = pure $ ST.defaultHandler handleEx


getIndexFile :: StaticFileDir -> SlateScottyAction
getIndexFile rootDir = ST.get "/" $ ST.file $ (T.unpack $ _fileDir rootDir) <> "/index.html"


getNotesEndpoint :: Connection -> SlateScottyAction
getNotesEndpoint = ST.get "/notes" . txSlateActionWithJson retrieveTopNotes


performSearchEndpoint :: Connection -> SlateScottyAction
performSearchEndpoint con =
    ST.get "/search" $ do
      query <- ST.param "q"
      txSlateActionWithJson (searchForNotes query) con


createNoteEndpoint :: Connection -> SlateScottyAction
createNoteEndpoint con =
  ST.post "/note" $ do
    (note :: IncomingNote) <- jsonErrorHandle
    noteIdE <- txSlateAction (saveNote note) con
    case noteIdE of
     (Left errorMessage)   -> withError errorMessage status400
     (Right noteIdVersion) -> maybe (jsonResponse created201 noteIdVersion) (const $ jsonResponse ok200 noteIdVersion) (_incomingNoteAndVersion note)


deleteNoteEndpoint :: Connection -> SlateScottyAction
deleteNoteEndpoint con =
  ST.delete "/note/:noteId" $ do
    noteId  <- mkNoteId <$> ST.param "noteId"
    deleteE <- txSlateAction (deleteNote noteId) con
    case deleteE of
      Left errorMessage  -> withError errorMessage status400
      (Right onlyNoteId) -> jsonResponse noContent204 onlyNoteId


txSlateActionWithJson :: ToJSON a => (Connection -> IO a) -> Connection -> SlateAction IO ()
txSlateActionWithJson action con = do
  value <- liftIO $ withTransaction con (action con)
  ST.json value


txSlateAction :: (Connection -> IO a) -> Connection -> SlateAction IO a
txSlateAction action con = liftIO $ withTransaction con (action con)


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


withError :: Monad m => DBError -> Status -> SlateAction m ()
withError dbError status = ST.json (dbErrorToString dbError) >> ST.status status


withPooledConnection :: P.Pool Connection -> (Connection -> IO a) -> IO a
withPooledConnection = P.withResource


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


deleteNote :: NoteId -> Connection -> IO (Either DBError OnlyNoteId)
deleteNote = deactivateNote
