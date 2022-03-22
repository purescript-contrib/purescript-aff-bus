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

data Bus (r :: Row Type) a = Bus (AVar a) (AVar (List (AVar a)))

type role Bus nominal representational

type BusR = BusR' ()

type BusR' r = Bus (read :: Cap | r)

type BusW = BusW' ()

type BusW' r = Bus (write :: Cap | r)

type BusRW = Bus (read :: Cap, write :: Cap)

-- | Creates a new bidirectional Bus which can be read from and written to.
make :: forall m a. MonadEffect m => m (BusRW a)
make = liftEffect do
  cell <- EffAvar.empty
  consumers <- EffAvar.new mempty
  launchAff_ $ void $ attempt $ forever do
    res <- AVar.take cell
    vars <- AVar.take consumers
    AVar.put Nil consumers
    sequence_ (foldl (\xs a -> AVar.put res a : xs) mempty vars)
  pure $ Bus cell consumers

-- | Blocks until a new value is pushed to the Bus, returning the value.
read :: forall a r. BusR' r a -> Aff a
read (Bus _ consumers) = do
  res' <- AVar.empty
  cs <- AVar.take consumers
  AVar.put (res' : cs) consumers
  AVar.take res'

-- | Pushes a new value to the Bus, yieldig immediately.
write :: forall a r. a -> BusW' r a -> Aff Unit
write a (Bus cell _) = AVar.put a cell

-- | Splits a bidirectional Bus into separate read and write Buses.
split :: forall a. BusRW a -> Tuple (BusR a) (BusW a)
split (Bus a b) = Tuple (Bus a b) (Bus a b)

-- | Kills the Bus and propagates the exception to all pending and future consumers.
kill :: forall a r. Exn.Error -> BusW' r a -> Aff Unit
kill err (Bus cell consumers) = unlessM (liftEffect $ EffAvar.isKilled <$> EffAvar.status cell) do
  AVar.kill err cell
  vars <- AVar.take consumers
  traverse_ (AVar.kill err) vars
  AVar.kill err consumers

-- | Synchronously checks whether a Bus has been killed.
isKilled :: forall m a r. MonadEffect m => BusR' r a -> m Boolean
isKilled (Bus cell _) = liftEffect $ EffAvar.isKilled <$> EffAvar.status cell
