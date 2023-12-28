module Effect.Promise
  ( Promise
  , runPromiseAE
  , runPromiseAE_
  ) where

import Prelude

import Control.Alternative (class Alt, class Alternative, class Plus)
import Control.Monad.Cont (ContT(..), runContT)
import Effect (Effect)
import Effect.AE (AE, runAE, runAE_)
import Effect.AE.Class (class AsyncTask)

foreign import data Promise :: Type -> Type

foreign import ffiPure :: forall a. a -> Promise a

foreign import ffiNew :: forall a. ((a -> Effect Unit) -> Effect Unit) -> Effect (Promise a)

foreign import ffiMap :: forall a b. (a -> b) -> Promise a -> Promise b

foreign import ffiThen :: forall a. Promise a -> (a -> Effect Unit) -> Effect Unit

foreign import ffiAlt :: forall a. Promise a -> Promise a -> Promise a

foreign import ffiApply :: forall a b. Promise (a -> b) -> Promise a -> Promise b

foreign import ffiNever :: forall a. Promise a

instance functorPromise :: Functor Promise where
  map = ffiMap

instance applyPromise :: Apply Promise where
  apply = ffiApply

instance applicativePromise :: Applicative Promise where
  pure = ffiPure

instance altPromise :: Alt Promise where
  alt = ffiAlt

instance plusPromise :: Plus Promise where
  empty = ffiNever

instance alternativePromise :: Alternative Promise

instance asyncTaskPromise :: AsyncTask Promise where
  forkTask  = ffiNew <<< runContT
  waitTask = ContT <<< ffiThen

runPromiseAE :: forall a. AE Promise a -> Effect (Promise a)
runPromiseAE = runAE

runPromiseAE_ :: forall a. AE Promise a -> Effect Unit
runPromiseAE_ = runAE_
