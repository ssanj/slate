{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DerivingStrategies  #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Server
       (
          -- DATA TYPES

          Except(..)
       ,  SlateAction

          -- FUNCTIONS

       ,  addStaticDirPolicy
       ,  createMiddleware
       ,  checkApiKey
       ,  handleEx
       ,  jsonErrorHandle
       ) where

import Model (ApiKey(..))

import Data.String            (fromString)
import Control.Monad          (void, when)
import Control.Monad.IO.Class (MonadIO)
import Data.List              (find)
import Data.Bool              (bool)
import Data.CaseInsensitive   (CI)
import Data.ByteString        (ByteString)
import Network.HTTP.Types     (status422, status400, status500)
import Data.Aeson             (FromJSON(..), eitherDecode, Result(..), fromJSON)
import Model                  (OutgoingError(..))

import qualified Network.Wai                   as W
import qualified Network.Wai.Middleware.Static as W
import qualified Network.HTTP.Types.Status     as H
import qualified Data.Text                     as T
import qualified Data.Text.Encoding            as T
import qualified Data.ByteString.Lazy          as BL
import qualified Web.Scotty.Trans              as ST
import qualified Data.Text.Encoding            as E


-- import qualified Data.CaseInsensitive    as CI

type SlateAction = ST.ActionT Except

data Except = MalformedJsonInput T.Text
            | InvalidInput T.Text
            | NoDataProvided T.Text
            | GenericError T.Text
    deriving stock (Show, Eq)


instance ST.ScottyError Except where
    stringError = GenericError . T.pack
    showError = fromString . show

addStaticDirPolicy :: W.Policy
addStaticDirPolicy = W.addBase "./static"

createMiddleware :: W.Policy -> W.Middleware
createMiddleware = W.staticPolicyWithOptions W.defaultOptions

checkApiKey :: ApiKey -> W.Middleware
checkApiKey apiKey baseApp = \req respF ->
  let maybeMatched = do
        (_, hv)     <- find ((apiKeyHeader ==) . fst) (W.requestHeaders req)
        headerValue <- bsToText hv
        void $ matchApiKey headerValue apiKey
  in maybe (respF noMatchingApiKey) (const $ baseApp req respF) maybeMatched

noMatchingApiKey :: W.Response
noMatchingApiKey = W.responseLBS H.status401 [] BL.empty

apiKeyHeader :: CI ByteString
apiKeyHeader = "x-api-key"

bsToText :: ByteString -> Maybe T.Text
bsToText = either (const Nothing) Just . T.decodeUtf8'

matchApiKey :: T.Text -> ApiKey -> Maybe T.Text
matchApiKey headerValue (ApiKey apiKeyValue) = bool Nothing (Just headerValue) (apiKeyValue == headerValue)

handleEx :: Monad m => Except -> SlateAction m ()
handleEx (MalformedJsonInput errorText) = ST.status status400 >> ST.json (OutgoingError 900 errorText)
handleEx (NoDataProvided errorText)     = ST.status status400 >> ST.json (OutgoingError 901 errorText)
handleEx (InvalidInput errorText)       = ST.status status422 >> ST.json (OutgoingError 902 errorText)
handleEx (GenericError errorText)       = ST.status status500 >> ST.json (OutgoingError 903 errorText)

jsonErrorHandle :: (FromJSON a, MonadIO m) => SlateAction m a
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
