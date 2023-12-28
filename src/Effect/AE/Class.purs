module Effect.AE.Class where

import Prelude

import Control.Alternative (class Alternative)
import Control.Monad.Cont (ContT)
import Effect (Effect)

class Alternative t <= AsyncTask t where
  runCPS :: forall a. ContT Unit Effect a -> Effect (t a)
  waitCPS :: forall a. t a -> ContT Unit Effect a

type AE2 = ContT Unit Effect

class Alternative t <= Fiber t where
  fork :: forall a. AE2 a -> AE2 (t a)
  wait :: forall a. t a -> AE2 a