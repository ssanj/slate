{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module ServerSpec where

import Data.Functor.Identity               (Identity(..))
import Test.Tasty.HUnit                    ((@?=), (@=?), Assertion, assertFailure)
import Server                              (handleEx, SlateAction, Except(..))
import Network.HTTP.Types                  (Status, status400, status422, status500)
import Model                               (OutgoingError(..))
import Data.Default.Class                   (def)
import Control.Monad.Trans.Except          (runExceptT)
import Control.Monad.Trans.Reader          (runReaderT)
import Control.Monad.Trans.State.Strict    (runStateT)

import qualified Web.Scotty.Internal.Types as ST
import qualified Data.Binary.Builder       as DBB
import qualified Data.Aeson                as A


unit_handleEx_MalformedJsonInput :: Assertion
unit_handleEx_MalformedJsonInput =
  let except        = MalformedJsonInput "some data"
      expectedError = OutgoingError 900 "some data"
  in assertExcept except (verifyOutputError status400 expectedError)


unit_handleEx_NoDataProvided :: Assertion
unit_handleEx_NoDataProvided =
  let except        = NoDataProvided "you gotsta give me something man"
      expectedError = OutgoingError 901 "you gotsta give me something man"
  in assertExcept except (verifyOutputError status400 expectedError)


unit_handleEx_InvalidInput :: Assertion
unit_handleEx_InvalidInput =
  let except        = InvalidInput "your input is invalid"
      expectedError = OutgoingError 902 "your input is invalid"
  in assertExcept except (verifyOutputError status422 expectedError)


unit_handleEx_GenericError :: Assertion
unit_handleEx_GenericError =
  let except        = GenericError "Some generic input"
      expectedError = OutgoingError 903 "Some generic input"
  in assertExcept except (verifyOutputError status500 expectedError)


verifyOutputError :: Status -> OutgoingError -> ST.ScottyResponse -> Assertion
verifyOutputError httpStatus expectedError response = do
  ST.srStatus response @?= httpStatus
  case ST.srContent response of
    (ST.ContentBuilder builder) ->
      let content       = DBB.toLazyByteString builder
          eitherContent = A.eitherDecode content :: Either String OutgoingError
      in either assertFailure (expectedError @=?) eitherContent

    _ -> assertFailure "expected ContentBuilder"


assertExcept :: Except -> (ST.ScottyResponse -> Assertion) -> Assertion
assertExcept ex assertResponse =
  let extAction  = handleEx ex :: SlateAction Identity () -- ActionT { runAM :: ExceptT (ActionError e) (ReaderT ActionEnv (StateT ScottyResponse Identity)) a }
      exT        = ST.runAM extAction --  ExceptT (ActionError e) (ReaderT ActionEnv (StateT ScottyResponse Identity)) a
      rT         = runExceptT exT -- ReaderT ActionEnv (StateT ScottyResponse Identity (Either (ActionError e) a
      env        = ST.Env undefined undefined undefined undefined undefined :: ST.ActionEnv
      st         = runReaderT rT env -- StateT ScottyResponse (Identity (Either (ActionError e)) a
      sr         = def :: ST.ScottyResponse
      mPair      = runStateT st sr -- Identity (Either (ActionError e) (a, ScottyResponse)
      (eitherPair, response) = runIdentity mPair -- Either (ActionError e) (a, ScottyResponse)
  in either (\_ -> assertFailure "got an error!") (\_ -> assertResponse response) eitherPair

