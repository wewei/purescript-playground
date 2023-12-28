module AEExamples.ApplicativeAE where

import Prelude

import Data.Time.Duration (class Duration, Seconds(..), convertDuration)
import Data.Tuple.Apply ((<&>))
import Effect (Effect)
import Effect.CPS (PFiber, Proc, delayedBy, fork, launchPFiber_, timing, wait)
import Effect.Class (class MonadEffect)
import Effect.Class.Console (log)

main :: Effect Unit
main = launchPFiber_ $ timing logSeconds do
    let w10 = log "Wait 1.0" `delayedBy` Seconds 1.0
        w05 = log "Wait 0.5" `delayedBy` Seconds 0.5
    void $ (w10 >>= const w05) <&> (w05 >>= const w10)
    let f = pure (_ + 1) `delayedBy` Seconds 0.3
    let a = pure 1 `delayedBy` Seconds 0.3
    r <- f <*> a
    log $ show r
    (x :: Proc Unit) <- fork w10
    (y :: Proc Unit) <- fork w10
    log "Forked"
    wait x
    wait y
    wait y
    log "Finished"

logSeconds :: forall m d. MonadEffect m => Duration d => d -> m Unit
logSeconds d = do
    let sec :: Seconds
        sec = convertDuration d
    log $ show sec