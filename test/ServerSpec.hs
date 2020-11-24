{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module ServerSpec where

import Data.Functor.Identity               (Identity(..))
import Test.Tasty.HUnit                    ((@?=), (@=?), Assertion, assertFailure)
import Server                              (handleEx, SlateAction, Except(..))
import Network.HTTP.Types                  (status400)
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
  let ex         = MalformedJsonInput "some data"
  in assertExcept ex assertMalformedInput
    where
        assertMalformedInput :: ST.ScottyResponse -> Assertion
        assertMalformedInput response = do
          ST.srStatus response @?= status400
          case ST.srContent response of
            (ST.ContentBuilder builder) ->
              let content       = DBB.toLazyByteString builder
                  eitherContent = A.eitherDecode content :: Either String OutgoingError
              in either assertFailure ((OutgoingError 900 "some data") @=?) eitherContent

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

