module Effect.AE.Timer where

import Prelude

import Data.DateTime.Instant (diff)
import Data.Int (round)
import Data.Newtype (unwrap)
import Data.Time.Duration (class Duration, Milliseconds, fromDuration)
import Effect.AE (AE, makeAE)
import Effect.AE.Class (class AsyncTask)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Now (now)
import Effect.Timer (setTimeout)

delay :: forall d. Duration d => d -> forall t. AsyncTask t => AE t Unit
delay d = makeAE \res -> do
  let ms = round $ unwrap (fromDuration d :: Milliseconds)
  void $ setTimeout ms (res unit)

delayedBy :: forall t a. AsyncTask t => AE t a -> forall d. Duration d => d -> AE t a
delayedBy ae d = delay d >>= const ae

timing :: forall m. MonadEffect m => (Milliseconds -> m Unit) -> forall a . m a -> m a
timing f ae = do
  start <- liftEffect now
  val   <- ae
  end   <- liftEffect now
  f (diff end start)
  pure val

