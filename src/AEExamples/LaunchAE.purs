module AEExamples.LaunchAE where

import Prelude

import Effect (Effect)
import Effect.Console (log)
import Effect.Promise (runPromiseAE, runPromiseAE_)
import SpecialLog (specialLog)

main :: Effect Unit
main = do
  log "This is an Effect computation (Effect monadic context).\n"

  void $ runPromiseAE do
    specialLog "This is an Aff computation (Aff monadic context)."

    specialLog "Aff provides the `launchAff` function that enables an \
               \Aff computation to run inside an Effect monadic context.\n"

  runPromiseAE_ do
    specialLog "`launchAff_` is just `void $ launchAff`.\n"

  log "Program finished."