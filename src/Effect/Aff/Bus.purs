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

import Control.Lazy (fix)
import Control.Monad.Rec.Class (forever)
import Data.Either (Either(..))
import Data.Foldable (foldl, sequence_, traverse_)
import Data.List (List(..), (:))
import Data.Tuple (Tuple(..))
import Effect.AVar as EffAVar
import Effect.Aff (Aff, Error, launchAff_, try)
import Effect.Aff.AVar (AVar)
import Effect.Aff.AVar as AVar
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Exception as Exn

data Cap

data Bus (r ∷ # Type) a = Bus (AVar (Either Error a)) (AVar (List (AVar a)))

type BusR = BusR' ()

type BusR' r = Bus (read ∷ Cap | r)

type BusW = BusW' ()

type BusW' r = Bus (write ∷ Cap | r)

type BusRW = Bus (read ∷ Cap, write ∷ Cap)

-- | Creates a new bidirectional Bus which can be read from and written to.
make ∷ ∀ m a. MonadEffect m ⇒ m (BusRW a)
make = liftEffect do
  cell ← EffAVar.empty
  consumers ← EffAVar.new mempty
  launchAff_ $ fix \loop -> do
    -- we `read` from `cell` instead of `take`, so that if error is written,
    -- `cell` can be killed, such that if there was any other `put` operations
    -- blocked, that will resolve with the error.
    resE ← AVar.read cell
    case resE of
      Left err -> do
        vars ← AVar.take consumers
        liftEffect do
          traverse_ (EffAVar.kill err) vars
          EffAVar.kill err consumers
          EffAVar.kill err cell
      Right res -> do
        void $ AVar.take cell
        vars ← AVar.take consumers
        AVar.put Nil consumers
        sequence_ (foldl (\xs a → AVar.put res a : xs) mempty vars)
        loop
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
write a (Bus cell _) = AVar.put (Right a) cell

-- | Splits a bidirectional Bus into separate read and write Buses.
split ∷ ∀ a. BusRW a → Tuple (BusR a) (BusW a)
split (Bus a b) = Tuple (Bus a b) (Bus a b)

-- | Kills the Bus and propagates the exception to all pending and future consumers.
-- | `kill` is idempotent and blocks until killing process is fully finishes, i.e.
-- | `kill err bus *> isKilled bus` will result with `true`.
kill ∷ ∀ a r. Exn.Error → BusW' r a → Aff Unit
kill err bus@(Bus cell consumers) = do
  unlessM (isKilled bus) do
    -- If there are multiple parallel processes executing `kill` at the same time,
    -- then without this try all of processes which are blocked bu put will be killed
    -- as part of handling first `put`. so we have this try to guaranty that kill is idempotent.
    void $ try $ AVar.put (Left err) cell
    -- Here we block until read from `cell` result's with the `error`,
    -- i.e. kill process was finished successfully.
    void $ try $ forever $ AVar.read cell

-- | Synchronously checks whether a Bus has been killed.
isKilled ∷ ∀ m a r. MonadEffect m ⇒ Bus r a → m Boolean
isKilled (Bus cell _) = liftEffect $ EffAVar.isKilled <$> EffAVar.status cell
