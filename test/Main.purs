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

import Control.Monad.Aff (Aff, Milliseconds(..), attempt, delay, forkAff, joinFiber, runAff_)
import Control.Monad.Aff.AVar (AVAR)
import Control.Monad.Aff.Bus as Bus
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (log, CONSOLE)
import Control.Monad.Eff.Exception (EXCEPTION, error, throwException)
import Control.Monad.Eff.Ref (REF, modifyRef, newRef, readRef, writeRef)
import Control.Monad.Error.Class (throwError)
import Data.Either (Either(..), either)

type Effects eff =
  ( console ∷ CONSOLE
  , avar ∷ AVAR
  , ref ∷ REF
  | eff
  )

test_readWrite ∷ ∀ eff. Bus.BusRW Int -> Aff (Effects eff) Boolean
test_readWrite bus = do
  ref ← liftEff $ newRef 0

  let
    proc = do
      res ← attempt (Bus.read bus)
      case res of
        Left e  → do
          liftEff $ modifyRef ref (_ + 100)
        Right n → do
          liftEff $ modifyRef ref (_ + n)
          proc

  f1 ← forkAff proc
  f2 ← forkAff proc

  Bus.write 1 bus
  Bus.write 2 bus
  Bus.write 3 bus

  -- without delay kill of bus interpats pending interactions with avar
  -- so we need to wait for some time to be sure that all actions are finished
  delay $ Milliseconds 10.0
  Bus.kill (error "Done") bus

  joinFiber f1
  joinFiber f2

  res <- liftEff $ readRef ref
  pure $ res == 212


main ∷ Eff (Effects (exception ∷ EXCEPTION)) Unit
main = do
  log "Testing read/write/kill..."
  runTest $ Bus.make >>= test_readWrite
  runTest $ (liftEff Bus.make) >>= test_readWrite
  where
  runTest t = do
    isFinishedRef <- newRef false
    runAff_ (isOk isFinishedRef) t
    runAff_ (either throwException pure) do
      delay (Milliseconds 100.0)
      isFinished <- liftEff $ readRef isFinishedRef
      unless isFinished $ throwError (error "Timeout")
    where
    isOk isFinishedRef = case _ of
      Left err -> throwException err
      Right res ->
        if res
          then do
            log "ok"
            writeRef isFinishedRef true
          else throwException $ error "failed"
