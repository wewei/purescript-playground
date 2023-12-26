module Effect.Promise where

import Prelude

import Control.Alt (class Alt)
import Control.Alternative (class Alternative)
import Control.Monad.Cont (ContT, runContT)
import Control.Plus (class Plus)
import Data.Tuple (Tuple(..))
import Data.Tuple.Nested ((/\))
import Effect (Effect)
import Effect.Class (class MonadEffect)

foreign import data Promise :: Type -> Type

foreign import ffiResolve :: forall a. a -> Promise a

resolve :: forall a. a -> Promise a
resolve = ffiResolve

foreign import ffiNew :: forall a. ((a -> Effect Unit) -> Effect Unit) -> Promise a

new :: forall a. ContT Unit Effect a -> Promise a
new = ffiNew <<< runContT

foreign import ffiThen :: forall a b. Promise a -> (a -> Effect (Promise b)) -> Promise b

infixl 6 ffiThen as $>>

foreign import ffiAlt :: forall a. Promise a -> Promise a -> Promise a

foreign import ffiBoth :: forall a b. (a -> b -> Tuple a b) -> Promise a -> Promise b -> Promise (Tuple a b)

both :: forall a b. Promise a -> Promise b -> Promise (Tuple a b)
both = ffiBoth Tuple

foreign import ffiNever :: forall a. Promise a

instance Functor Promise where
  map f p = p $>> pure <<< pure <<< f

instance Apply Promise where
  apply pF pA = (\(f /\ a) -> f a) <$> both pF pA

instance Applicative Promise where
  pure = resolve

instance Bind Promise where
  bind pA f = pA $>> (pure <<< f)

instance Monad Promise

instance Alt Promise where
  alt = ffiAlt

instance Plus Promise where
  empty = ffiNever

instance Alternative Promise

instance MonadEffect Promise where
  liftEffect = ffiNew <<< bind
