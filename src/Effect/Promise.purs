module Effect.Promise where

import Prelude

import Effect (Effect)

foreign import data Promise :: Type -> Type

foreign import newPromise :: forall a. ((a -> Effect Unit) -> Effect Unit) -> Promise a

-- foreign import then