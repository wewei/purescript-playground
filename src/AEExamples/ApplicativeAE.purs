module AEExamples.ApplicativeAE where

import Prelude

import Data.Time.Duration (class Duration, Seconds(..), convertDuration)
import Data.Tuple.Apply ((<&>))
import Effect (Effect)
import Effect.Class (class MonadEffect)
import Effect.Class.Console (log)
import Effect.CPS (delayedBy, launchPFiber_, timing)

main :: Effect Unit
main = launchPFiber_ $ timing logSeconds do
    let w3 = log "Wait 3.0" `delayedBy` Seconds 3.0
        w2 = log "Wait 2.0" `delayedBy` Seconds 2.0
    void $ (w3 >>= const w2) <&> (w2 >>= const w3)
    let f = pure (_ + 1) `delayedBy` Seconds 2.0
    let a = pure 1 `delayedBy` Seconds 2.0
    r <- f <*> a
    log $ show r

logSeconds :: forall m d. MonadEffect m => Duration d => d -> m Unit
logSeconds d = do
    let sec :: Seconds
        sec = convertDuration d
    log $ show sec