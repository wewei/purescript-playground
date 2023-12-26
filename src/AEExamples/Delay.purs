module AEExamples.Delay where

import Prelude

import Data.Time.Duration (Milliseconds(..))
import Effect (Effect)
import Effect.AE (runAE_)
import Effect.AE.Timer (delay)
import SpecialLog (specialLog)

main :: Effect Unit
main = runAE_ do
  specialLog "Let's print something to the console and then \
             \wait 1 second before printing another thing."

  delay $ Milliseconds 1000.0 -- 1 second

  specialLog "1 second has passed."
  specialLog "Program finished."