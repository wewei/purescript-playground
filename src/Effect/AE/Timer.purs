module Effect.AE.Timer where

import Prelude

import Data.DateTime.Instant (diff)
import Data.Int (round)
import Data.Newtype (unwrap)
import Data.Time.Duration (class Duration, Milliseconds, fromDuration)
import Effect.AE (AE, makeAE)
import Effect.Class (liftEffect)
import Effect.Now (now)
import Effect.Timer (setTimeout)

delay :: forall d. Duration d => d -> AE Unit
delay d = makeAE \res -> do
  let ms = round $ unwrap (fromDuration d :: Milliseconds)
  void $ setTimeout ms (res unit)

delayedBy :: forall a. AE a -> forall d. Duration d => d -> AE a
delayedBy ae d = delay d >>= const ae

timing :: (Milliseconds -> AE Unit) -> forall a . AE a -> AE a
timing f ae = do
  start <- liftEffect now
  val   <- ae
  end   <- liftEffect now
  f (diff end start)
  pure val

