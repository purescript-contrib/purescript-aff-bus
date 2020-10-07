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

import Control.Monad.Error.Class (throwError)
import Control.Parallel (parSequence_)
import Data.Bifunctor (lmap)
import Data.Either (Either(..), either)
import Effect (Effect)
import Effect.Aff (Aff, Milliseconds(..), attempt, delay, forkAff, joinFiber, runAff_)
import Effect.Aff.Bus as Bus
import Effect.Class (liftEffect)
import Effect.Console (log)
import Effect.Exception (error, throwException)
import Effect.Ref as Ref
import Test.Assert (assertEqual', assertTrue')

test_readWrite ∷ Bus.BusRW Int -> Aff Unit
test_readWrite bus = do
  ref ← liftEffect $ Ref.new []

  let
    proc = do
      res ← attempt (Bus.read bus)
      void $ liftEffect $ Ref.modify (_ <> [res]) ref
      either (const $ pure unit) (const proc) res

  f1 ← forkAff proc
  f2 ← forkAff proc

  Bus.write 1 bus
  Bus.write 2 bus
  Bus.write 3 bus

  let err = error "Done"
  -- killing in parallel must be safe
  parSequence_
    [ Bus.kill err bus
    , Bus.kill err bus
    , Bus.kill err bus
    ]
  isKilled <- Bus.isKilled bus
  liftEffect $ assertTrue' "`isKilled` immediately after `kill` results `true`" isKilled
  
  -- kill is idempotent
  Bus.kill err bus

  readRes <- attempt (Bus.read bus)
  liftEffect $ assertEqual' "`read` from killed bus should resolve with same error which was used to `kill`"
    {actual: lmap show readRes, expected: Left $ show err}

  joinFiber f1
  joinFiber f2

  res <- liftEffect $ Ref.read ref
  liftEffect $ assertEqual' "`res` should be as expected"
    {actual: lmap show <$> res, expected: [Right 1, Right 1, Right 2, Right 2, Right 3, Right 3, Left $ show err, Left $ show err]}


main ∷ Effect Unit
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
      Right res -> do
        log "ok"
        Ref.write true isFinishedRef
