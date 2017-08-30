{-
Copyright 2016 SlamData, Inc.

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
import Control.Monad.Aff (Aff, forkAff, launchAff, joinFiber, attempt)
import Control.Monad.Aff.AVar (AVAR)
import Control.Monad.Aff.Bus as Bus
import Control.Monad.Aff.Console (log)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (CONSOLE)
import Control.Monad.Eff.Exception (EXCEPTION, error)
import Control.Monad.Eff.Ref (REF, newRef, readRef, modifyRef)
import Control.Monad.Error.Class (throwError)
import Data.Either (Either(..))

type Effects eff =
  ( console ∷ CONSOLE
  , avar ∷ AVAR
  , ref ∷ REF
  | eff
  )

assert ∷ ∀ eff. Boolean → Aff eff Unit
assert a = unless a (throwError (error "Assertion failed"))

test_readWrite ∷ ∀ eff. Aff (Effects eff) Unit
test_readWrite = do
  bus ← Bus.make
  ref ← liftEff $ newRef 0

  let
    proc s = do
      res ← attempt (Bus.read bus)
      case res of
        Left e  → do
          liftEff $ modifyRef ref (_ + 100)
        Right n → do
          liftEff $ modifyRef ref (_ + n)
          proc s

  f1 ← forkAff $ proc "a"
  f2 ← forkAff $ proc "b"

  Bus.write 1 bus
  Bus.write 2 bus
  Bus.write 3 bus
  Bus.kill (error "Done") bus

  joinFiber f1
  joinFiber f2

  res <- liftEff $ readRef ref
  assert (res == 212)
  log "OK"

main ∷ Eff (Effects (exception ∷ EXCEPTION)) Unit
main = void $ launchAff do
  log "Testing read/write/kill..."
  test_readWrite
