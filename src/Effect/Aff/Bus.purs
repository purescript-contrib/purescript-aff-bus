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

module Effect.Aff.Bus
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

import Effect.Aff (Aff, attempt, launchAff_)
import Effect.Aff.AVar (AVar)
import Effect.Aff.AVar as AVar
import Effect.AVar as EffAvar
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Exception as Exn
import Control.Monad.Rec.Class (forever)
import Data.Foldable (foldl, sequence_, traverse_)
import Data.List (List(..), (:))
import Data.Tuple (Tuple(..))

data Cap

data Bus (r ∷ Row Type) a = Bus (AVar a) (AVar (List (AVar a)))

type BusR = BusR' ()

type BusR' r = Bus (read ∷ Cap | r)

type BusW = BusW' ()

type BusW' r = Bus (write ∷ Cap | r)

type BusRW = Bus (read ∷ Cap, write ∷ Cap)

-- | Creates a new bidirectional Bus which can be read from and written to.
make ∷ ∀ m a. MonadEffect m ⇒ m (BusRW a)
make = liftEffect do
  cell ← EffAvar.empty
  consumers ← EffAvar.new mempty
  launchAff_ $ attempt $ forever do
    res ← AVar.take cell
    vars ← AVar.take consumers
    AVar.put Nil consumers
    sequence_ (foldl (\xs a → AVar.put res a : xs) mempty vars)
  pure $ Bus cell consumers

-- | Blocks until a new value is pushed to the Bus, returning the value.
read ∷ ∀ a r. BusR' r a → Aff a
read (Bus _ consumers) = do
  res' ← AVar.empty
  cs ← AVar.take consumers
  AVar.put (res' : cs) consumers
  AVar.take res'

-- | Pushes a new value to the Bus, yieldig immediately.
write ∷ ∀ a r. a → BusW' r a → Aff Unit
write a (Bus cell _) = AVar.put a cell

-- | Splits a bidirectional Bus into separate read and write Buses.
split ∷ ∀ a. BusRW a → Tuple (BusR a) (BusW a)
split (Bus a b) = Tuple (Bus a b) (Bus a b)

-- | Kills the Bus and propagates the exception to all pending and future consumers.
kill ∷ ∀ a r. Exn.Error → BusW' r a → Aff Unit
kill err (Bus cell consumers) = unlessM (liftEffect $ EffAvar.isKilled <$> EffAvar.status cell) do
  AVar.kill err cell
  vars ← AVar.take consumers
  traverse_ (AVar.kill err) vars
  AVar.kill err consumers

-- | Synchronously checks whether a Bus has been killed.
isKilled ∷ ∀ m a r. MonadEffect m ⇒ BusR' r a → m Boolean
isKilled (Bus cell _) = liftEffect $ EffAvar.isKilled <$> EffAvar.status cell
