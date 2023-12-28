module Effect.AE.Class where

import Prelude

import Control.Alternative (class Alternative)
import Control.Monad.Cont (ContT)
import Effect (Effect)

type AE2 a = ContT Unit Effect a

class Alternative t <= AsyncTask t where
  forkTask :: forall a. AE2 a -> Effect (t a)
  waitTask :: forall a. t a -> AE2 a
