module Effect.AE where

import Prelude

import Control.Alternative (class Alt, class Alternative, class Plus, alt, empty)
import Control.Apply (lift2)
import Effect (Effect)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Promise (Promise, promiseNew, promiseThen)

newtype AE a = AE (Effect (Promise a))

runAE :: forall a. AE a -> Effect (Promise a)
runAE (AE eff) = eff

runAE_ :: forall a. AE a -> Effect Unit
runAE_ = void <<< runAE

makeAE :: forall a. ((a -> Effect Unit) -> Effect Unit) -> AE a
makeAE = AE <<< promiseNew

liftP0 :: forall a. Promise a -> AE a
liftP0 = AE <<< pure

liftP1 :: forall a b. (Promise a -> Promise b) -> AE a -> AE b
liftP1 f = AE <<< map f <<< runAE

liftP2 :: forall a b c. (Promise a -> Promise b -> Promise c) -> AE a -> AE b -> AE c
liftP2 f aeA aeB = AE $ lift2 f (runAE aeA) (runAE aeB)

instance functorAE :: Functor AE where
  map = liftP1 <<< map

instance applyAE :: Apply AE where
  apply = liftP2 apply

instance applicativeAE :: Applicative AE where
  pure = liftP0 <<< pure

instance bindAE :: Bind AE where
  bind ae f = AE $ do
    p <- runAE ae
    promiseThen p $ runAE <<< f

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

wait :: forall a. Promise a -> AE a
wait p = liftP0 p

fork :: forall m a. MonadEffect m => AE a -> m (Promise a)
fork = liftEffect <<< runAE
