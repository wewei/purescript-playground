module Effect.Promise where

import Prelude

import Control.Alternative (class Alt, class Alternative, class Plus, alt, empty)
import Control.Apply (lift2)
import Control.Monad.Cont (ContT(..), runContT)
import Effect (Effect)
import Effect.Class (class MonadEffect)

foreign import data Promise :: Type -> Type

foreign import ffiPure :: forall a. a -> Promise a

foreign import ffiNew :: forall a. ((a -> Effect Unit) -> Effect Unit) -> Effect (Promise a)

foreign import ffiRun :: forall a. Promise a -> (a -> Effect Unit) -> Effect Unit
runPromise :: forall a. Promise a -> ContT Unit Effect a
runPromise = ContT <<< ffiRun

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

newtype AE a = AE (Effect (Promise a))

runAE :: forall a. AE a -> Effect (Promise a)
runAE (AE eff) = eff

runAE_ :: forall a. AE a -> Effect Unit
runAE_ = void <<< runAE

liftP0 :: forall a. Promise a -> AE a
liftP0 = AE <<< pure

liftP1 :: forall a b. (Promise a -> Promise b) -> AE a -> AE b
liftP1 f = AE <<< map f <<< runAE

liftP2 :: forall a b c. (Promise a -> Promise b -> Promise c) -> AE a -> AE b -> AE c
liftP2 f aeA aeB = AE $ lift2 f (runAE aeA) (runAE aeB)

makeAE :: forall a. ContT Unit Effect a -> AE a
makeAE = AE <<< ffiNew <<< runContT

instance functorAE :: Functor AE where
  map = liftP1 <<< map

instance applyAE :: Apply AE where
  apply = liftP2 apply

instance applicativeAE :: Applicative AE where
  pure = liftP0 <<< pure

instance bindAE :: Bind AE where
  bind ae f = AE $ do
    p <- runAE ae
    runAE $ p `next` f

instance Monad AE

instance altAE :: Alt AE where
  alt = liftP2 alt

instance plusAE :: Plus AE where
  empty = AE $ pure empty

instance alternativeAE :: Alternative AE

instance monadEffectAE :: MonadEffect AE where
  liftEffect eff = AE do
    a <- eff
    pure $ pure a

next :: forall a b. Promise a -> (a -> AE b) -> AE b
next pA f = AE $ ffiThen pA (runAE <<< f)

infixl 1 next as ~~>
