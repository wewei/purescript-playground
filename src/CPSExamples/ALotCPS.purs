module CPSExamples.ALotCPS where

import Prelude

import CPSExamples.Utility (logSeconds)
import Data.List.Lazy (List, range)
import Data.Time.Duration (Seconds(..))
import Data.Traversable (for)
import Effect (Effect)
import Effect.CPS (PFiber, launchPFiber_, timing, fork, wait, delay)
import Effect.Class.Console (log)

main :: Effect Unit
main = launchPFiber_ $ timing logSeconds do
    let w10 = delay $ Seconds 1.0
    (fibs :: List (PFiber Unit)) <- for (range 1 5000)
        \n -> fork (w10 >>= const (log $ "Fiber #" <> show n))
    for fibs wait
