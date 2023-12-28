module Effect.Monad.CPS.Timer where

import Prelude

import Data.DateTime.Instant (diff)
import Data.Int (round)
import Data.Newtype (unwrap)
import Data.Time.Duration (class Duration, Milliseconds, fromDuration)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Monad.CPS.Proc (Proc, proc)
import Effect.Now (now)
import Effect.Timer (setTimeout)

delay :: forall d. Duration d => d -> Proc Unit
delay d = proc \r -> do let ms = round $ unwrap (fromDuration d :: Milliseconds)
                        void $ setTimeout ms (r unit)

delayedBy :: forall a. Proc a -> forall d. Duration d => d -> Proc a
delayedBy prc d = delay d >>= const prc

timing :: forall m. MonadEffect m => (Milliseconds -> m Unit) -> forall a . m a -> m a
timing f ae = do
  start <- liftEffect now
  val   <- ae
  end   <- liftEffect now
  f (diff end start)
  pure val
