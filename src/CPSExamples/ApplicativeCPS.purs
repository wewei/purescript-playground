module CPSExamples.ApplicativeCPS where

import Prelude

import Control.Alternative ((<|>))
import Data.Time.Duration (class Duration, Seconds(..), convertDuration)
import Data.Tuple.Apply ((<&>))
import Effect (Effect)
import Effect.CPS (PFiber, Proc, delayedBy, fork, launchPFiber_, timing, wait, delay)
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
    (x :: Proc Unit) <- fork $ w10 >>= const (log "Complete Proc x")
    (y :: PFiber Unit) <- fork $ w10 >>= const (log "Complete PFiber y")
    log "Forked"
    wait x
    log "Wait 1st Proc x"
    wait x
    log "Wait 2nd Proc x"
    wait y
    log "Wait 1st PFiber y"
    wait y
    log "Wait 2nd PFiber y"
    let prcL = do delay $ Seconds 0.2
                  log "Running Proc Left"
                  pure "Left"
    let prcR = do delay $ Seconds 0.3
                  log "Running Proc Right"
                  pure "Right"
    (val :: PFiber String) <- fork (prcL <|> prcR)
    wait val >>= ("Result: " <> _) >>> log
    log "Program Finished"

logSeconds :: forall m d. MonadEffect m => Duration d => d -> m Unit
logSeconds d = do
    let sec :: Seconds
        sec = convertDuration d
    log $ show sec