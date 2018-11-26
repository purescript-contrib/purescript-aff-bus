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

module Control.Monad.Aff.Bus
  ( make
  , read
  , write
  , split
  , kill
  , isKilled
  , Cap
  , Bus
  , BusRW
  , BusR
  , BusR'
  , BusW
  , BusW'
  ) where

import Prelude

import Control.Monad.Aff (Aff, attempt, launchAff_)
import Control.Monad.Aff.AVar (AVAR, AVar, killVar, makeEmptyVar, putVar, takeVar)
import Control.Monad.Eff.AVar (isKilledVar)
import Control.Monad.Eff.AVar as EffAvar
import Control.Monad.Eff.Class (class MonadEff, liftEff)
import Control.Monad.Eff.Exception as Exn
import Control.Monad.Rec.Class (forever)
import Data.Foldable (foldl, sequence_, traverse_)
import Data.List (List(..), (:))
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
make ∷ ∀ m eff a. MonadEff (avar ∷ AVAR | eff) m ⇒ m (BusRW a)
make = liftEff do
  cell ← EffAvar.makeEmptyVar
  consumers ← EffAvar.makeVar mempty
  launchAff_ $ attempt $ forever do
    res ← takeVar cell
    vars ← takeVar consumers
    putVar Nil consumers
    sequence_ (foldl (\xs a → putVar res a : xs) mempty vars)
  pure $ Bus cell consumers

-- | Blocks until a new value is pushed to the Bus, returning the value.
read ∷ ∀ eff a r. BusR' r a → Aff (avar ∷ AVAR | eff) a
read (Bus _ consumers) = do
  res' ← makeEmptyVar
  cs ← takeVar consumers
  putVar (res' : cs) consumers
  takeVar res'

-- | Pushes a new value to the Bus, yieldig immediately.
write ∷ ∀ eff a r. a → BusW' r a → Aff (avar ∷ AVAR | eff) Unit
write a (Bus cell _) = putVar a cell

-- | Splits a bidirectional Bus into separate read and write Buses.
split ∷ ∀ a. BusRW a → Tuple (BusR a) (BusW a)
split (Bus a b) = Tuple (Bus a b) (Bus a b)

-- | Kills the Bus and propagates the exception to all pending and future consumers.
kill ∷ ∀ eff a r. Exn.Error → BusW' r a → Aff (avar ∷ AVAR | eff) Unit
kill err (Bus cell consumers) = unlessM (liftEff $ isKilledVar cell) do
  killVar err cell
  vars ← takeVar consumers
  traverse_ (killVar err) vars
  killVar err consumers

-- | Synchronously checks whether a Bus has been killed.
isKilled ∷ ∀ m eff a r. MonadEff (avar ∷ AVAR | eff) m ⇒ BusR' r a → m Boolean
isKilled (Bus cell _) = liftEff $ isKilledVar cell
