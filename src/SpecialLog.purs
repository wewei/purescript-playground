module SpecialLog where

import Prelude

import Effect.Class (class MonadEffect, liftEffect)
import Effect.Console (log)

specialLog :: forall m. MonadEffect m =>  String -> m Unit
specialLog = liftEffect <<<  log

