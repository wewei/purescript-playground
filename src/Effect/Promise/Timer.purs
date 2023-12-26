module Effect.Promise.Timer where

import Prelude

import Data.Int (round)
import Data.Newtype (unwrap)
import Data.Time.Duration (class Duration, Milliseconds, fromDuration)
import Effect.Promise (AE(..), ffiNew)
import Effect.Timer (setTimeout)

delay :: forall d. Duration d => d -> AE Unit
delay d = AE $ ffiNew \res -> do
  let ms = round $ unwrap (fromDuration d :: Milliseconds)
  void $ setTimeout ms (res unit)

