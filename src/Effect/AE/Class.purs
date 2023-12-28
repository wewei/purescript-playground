module Effect.AE.Class where

import Prelude

import Control.Alternative (class Alternative)
import Control.Monad.Cont (ContT)
import Effect (Effect)

class Alternative t <= AsyncTask t where
  forkTask :: forall a. ContT Unit Effect a -> Effect (t a)
  waitTask :: forall a. t a -> ContT Unit Effect a
