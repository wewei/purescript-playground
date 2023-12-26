module Effect.Promise
  ( Promise
  , promiseNew
  , promiseThen
  )
  where

import Prelude

import Control.Alternative (class Alt, class Alternative, class Plus)
import Effect (Effect)

foreign import data Promise :: Type -> Type

foreign import ffiPure :: forall a. a -> Promise a

foreign import ffiNew :: forall a. ((a -> Effect Unit) -> Effect Unit) -> Effect (Promise a)

foreign import ffiMap :: forall a b. (a -> b) -> Promise a -> Promise b

foreign import ffiThen :: forall a b. Promise a -> (a -> Effect (Promise b)) -> Effect (Promise b)

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

promiseNew :: forall a. ((a -> Effect Unit) -> Effect Unit) -> Effect (Promise a)
promiseNew = ffiNew

promiseThen :: forall a b. Promise a -> (a -> Effect (Promise b)) -> Effect (Promise b)
promiseThen = ffiThen
