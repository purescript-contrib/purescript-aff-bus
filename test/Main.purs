{-
Copyright 2018 SlamData, Inc.

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.
-}

module Test.Main where

import Prelude

import Effect.Aff (Aff, Milliseconds(..), attempt, delay, forkAff, joinFiber, runAff_)
import Effect.Aff.Bus as Bus
import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Console (log)
import Effect.Exception (error, throwException)
import Effect.Ref as Ref
import Control.Monad.Error.Class (throwError)
import Data.Either (Either(..), either)

test_readWrite :: Bus.BusRW Int -> Aff Boolean
test_readWrite bus = do
  ref <- liftEffect $ Ref.new 0

  let
    proc = do
      res <- attempt (Bus.read bus)
      case res of
        Left _ -> do
          void $ liftEffect $ Ref.modify (_ + 100) ref
        Right n -> do
          void $ liftEffect $ Ref.modify (_ + n) ref
          proc

  f1 <- forkAff proc
  f2 <- forkAff proc

  Bus.write 1 bus
  Bus.write 2 bus
  Bus.write 3 bus

  -- without delay kill of bus interpats pending interactions with avar
  -- so we need to wait for some time to be sure that all actions are finished
  delay $ Milliseconds 10.0
  let err = error "Done"
  Bus.kill err bus
  attempt (Bus.read bus) >>= case _ of
    Left err' | show err' == show err -> pure unit
    _ -> throwError $ error "read from killed bus should resolve with same error which was used to kill"
  unlessM (Bus.isKilled bus) $ throwError $ error "isKilled must return true as bus was killed"

  joinFiber f1
  joinFiber f2

  res <- liftEffect $ Ref.read ref
  pure $ res == 212

main :: Effect Unit
main = do
  log "Testing read/write/kill..."
  runTest $ Bus.make >>= test_readWrite
  runTest $ (liftEffect Bus.make) >>= test_readWrite
  where
  runTest t = do
    isFinishedRef <- Ref.new false
    runAff_ (isOk isFinishedRef) t
    runAff_ (either throwException pure) do
      delay (Milliseconds 100.0)
      isFinished <- liftEffect $ Ref.read isFinishedRef
      unless isFinished $ throwError (error "Timeout")
    where
    isOk isFinishedRef = case _ of
      Left err -> throwException err
      Right res ->
        if res then do
          log "ok"
          Ref.write true isFinishedRef
        else throwException $ error "failed"
