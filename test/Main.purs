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
import Control.Monad.Aff (Aff, forkAff, launchAff, attempt)
import Control.Monad.Aff.AVar (AVAR, makeVar', modifyVar, peekVar)
import Control.Monad.Aff.Bus as Bus
import Control.Monad.Aff.Console (log)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE)
import Control.Monad.Eff.Exception (EXCEPTION, error, message)
import Control.Monad.Error.Class (throwError)
import Data.Either (Either(..))

type Effects eff =
  ( console ∷ CONSOLE
  , avar ∷ AVAR
  | eff
  )

assert ∷ ∀ eff. Boolean → Aff eff Unit
assert a = unless a (throwError (error "Assertion failed"))

test_readWrite ∷ ∀ eff. Aff (Effects eff) Unit
test_readWrite = do
  bus ← Bus.make
  avar ← makeVar' 0

  let
    proc = do
      res ← attempt (Bus.read bus)
      case res of
        Left e  → do
          modifyVar (_ + 100) avar
          log (message e)
        Right n → do
          modifyVar (_ + n) avar
          proc

  void $ forkAff proc
  void $ forkAff proc

  Bus.write 1 bus
  Bus.write 2 bus
  Bus.write 3 bus
  Bus.kill (error "Done") bus

  assert <<< eq 212 =<< peekVar avar
  log "OK"

main ∷ Eff (Effects (exception ∷ EXCEPTION)) Unit
main = void $ launchAff do
  log "Testing read/write/kill..."
  test_readWrite
