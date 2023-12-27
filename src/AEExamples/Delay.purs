module AEExamples.Delay where

import Prelude

import Data.Time.Duration (Milliseconds(..))
import Effect (Effect)
import Effect.AE.Timer (delay)
import Effect.Promise (runPromiseAE_)
import SpecialLog (specialLog)

main :: Effect Unit
main = runPromiseAE_ do
  specialLog "Let's print something to the console and then \
             \wait 1 second before printing another thing."

  delay $ Milliseconds 1000.0 -- 1 second

  specialLog "1 second has passed."
  specialLog "Program finished."