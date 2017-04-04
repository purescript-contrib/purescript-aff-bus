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

module Control.Monad.Aff.Bus
  ( make
  , read
  , read'
  , write
  , split
  , kill
  , Cap
  , Bus
  , BusRW
  , BusR
  , BusR'
  , BusW
  , BusW'
  ) where

import Prelude
import Control.Monad.Aff (forkAff)
import Control.Monad.Aff.AVar (AffAVar, AVar, makeVar', makeVar, takeVar, putVar, modifyVar, killVar)
import Control.Monad.Eff.Exception as Exn
import Control.Monad.Rec.Class (forever)
import Data.Foldable (foldl, sequence_, traverse_)
import Data.List (List, (:))
import Data.Monoid (mempty)
import Data.Tuple (Tuple(..))

data Cap

data Bus (r ∷ # Type) a = Bus (AVar a) (AVar (List (AVar a)))

type BusR = BusR' ()

type BusR' r = Bus (read ∷ Cap | r)

type BusW = BusW' ()

type BusW' r = Bus (write ∷ Cap | r)

type BusRW = Bus (read ∷ Cap, write ∷ Cap)

-- | Creates a new bidirectional Bus which can be read from and written to.
make ∷ ∀ eff a. AffAVar eff (BusRW a)
make = do
  cell ∷ AVar a ← makeVar
  consumers ∷ AVar (List (AVar a)) ← makeVar' mempty
  _ ← forkAff $ forever do
    res ← takeVar cell
    vars ← takeVar consumers
    putVar consumers mempty
    sequence_ (foldl (\xs a → putVar a res : xs) mempty vars)
  pure $ Bus cell consumers

-- | Blocks until a new value is pushed to the Bus, returning the value.
read ∷ ∀ eff a r. BusR' r a → AffAVar eff a
read = takeVar <=< read'

-- | Returns an AVar that will yield a one-time value.
read' ∷ ∀ eff a r. BusR' r a → AffAVar eff (AVar a)
read' (Bus _ consumers) = do
  res' ← makeVar
  modifyVar (res' : _) consumers
  pure res'

-- | Pushes a new value to the Bus, yieldig immediately.
write ∷ ∀ eff a r. a → BusW' r a → AffAVar eff Unit
write a (Bus cell _) = putVar cell a

-- | Splits a bidirectional Bus into separate read and write Buses.
split ∷ ∀ a. BusRW a → Tuple (BusR a) (BusW a)
split (Bus a b) = Tuple (Bus a b) (Bus a b)

-- | Kills the Bus and propagates the exception to all consumers.
kill ∷ ∀ eff a r. Exn.Error → BusW' r a → AffAVar eff Unit
kill err (Bus cell consumers) = do
  killVar cell err
  vars ← takeVar consumers
  killVar consumers err
  traverse_ (flip killVar err) vars
