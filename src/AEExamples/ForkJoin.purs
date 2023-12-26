module AEExamples.ForkJoin where

import Prelude

import Data.Time.Duration (Milliseconds(..))
import Effect (Effect)
import Effect.AE (fork, runAE_, wait)
import Effect.AE.Timer (delay)
import SpecialLog (specialLog)

main :: Effect Unit
main = runAE_ do
  let
    fiber1 = "Fiber 1"
    fiber2 = "Fiber 2"
    fiber3 = "Fiber 3"

  specialLog "Let's run multiple computations concurrently. Then, \
             \we'll use `joinFiber` to ensure all computations have \
             \finished before we do another computation."

  firstFiber <- fork $ do
    specialLog $ fiber1 <> ": Waiting for 1 second until completion."
    delay $ Milliseconds 1000.0
    specialLog $ fiber1 <> ": Finished computation."

  secondFiber <- fork $ do
    specialLog $ fiber2 <> ": Computation 1 (takes 300 ms)."
    delay $ Milliseconds 300.0

    specialLog $ fiber2 <> ": Computation 2 (takes 300 ms)."
    delay $ Milliseconds 300.0

    specialLog $ fiber2 <> ": Computation 3 (takes 500 ms)."
    delay $ Milliseconds 500.0
    specialLog $ fiber2 <> ": Finished computation."

  thirdFiber <- fork $ do
    specialLog $ fiber3 <> ": Nothing to do. Just return immediately."
    specialLog $ fiber3 <> ": Finished computation."

  wait firstFiber
  specialLog $ fiber1 <> " has finished. Now joining on " <> fiber2

  wait secondFiber
  specialLog $ fiber2 <> " has finished. Now joining on " <> fiber3

  wait thirdFiber
  specialLog $ fiber3 <> " has finished. All fibers have finished their \
                         \computation."

  specialLog "Program finished."