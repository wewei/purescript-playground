module Effect.AE.Class where

import Prelude

import Control.Alternative (class Alternative)
import Effect (Effect)

class Alternative t <= AsyncTask t where
  new :: forall a. ((a -> Effect Unit) -> Effect Unit) -> Effect (t a)
  next :: forall a. t a -> forall b.(a -> Effect (t b)) -> Effect (t b)

