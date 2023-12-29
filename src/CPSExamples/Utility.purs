module CPSExamples.Utility where

import Prelude

import Data.Time.Duration (class Duration, Seconds, convertDuration)
import Effect.Class (class MonadEffect)
import Effect.Class.Console (log)

logSeconds :: forall m d. MonadEffect m => Duration d => d -> m Unit
logSeconds d = do
    let sec :: Seconds
        sec = convertDuration d
    log $ show sec