module AEExamples.Main where

import Prelude

import Data.DateTime.Instant (diff)
import Data.Time.Duration (class Duration, Seconds(..))
import Data.Tuple.Apply ((<&>))
import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Class.Console (log)
import Effect.Now (now)
import Effect.Promise (AE, runAE_)
import Effect.Promise.Timer (delay)

delayBy :: forall a. AE a -> forall d. Duration d => d -> AE a 
delayBy ae d = delay d >>= const ae

timing :: forall a. AE a -> AE a
timing ae = do
    s <- liftEffect now
    a <- ae
    e <- liftEffect now
    let d :: Seconds
        d = diff e s
    log <<< show $ d
    pure a


main :: Effect Unit
main = runAE_ $ timing do
    let w3 = log "Wait 3.0" `delayBy` Seconds 3.0
        w2 = log "Wait 2.0" `delayBy` Seconds 2.0
    void $ (w3 >>= const w2) <&> (w2 >>= const w3)
    let f = pure (_ + 1) `delayBy` Seconds 2.0
    let a = pure 1 `delayBy` Seconds 2.0
    r <- f <*> a
    log $ show r
    
