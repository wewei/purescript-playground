module Effect.AE where

import Prelude

import Control.Alternative (class Alt, class Alternative, class Plus, alt, empty)
import Control.Apply (lift2)
import Effect (Effect)
import Effect.AE.Class (class AsyncTask, new, next)
import Effect.Class (class MonadEffect, liftEffect)

newtype AE :: forall k. (k -> Type) -> k -> Type
newtype AE t a = AE (Effect (t a))

runAE :: forall t a. AsyncTask t => AE t a -> Effect (t a)
runAE (AE eff) = eff

runAE_ :: forall t a. AsyncTask t => AE t a -> Effect Unit
runAE_ = void <<< runAE

makeAE :: forall t a. AsyncTask t => ((a -> Effect Unit) -> Effect Unit) -> AE t a
makeAE = AE <<< new

liftP0 :: forall t a. AsyncTask t => t a -> AE t a
liftP0 = AE <<< pure

liftP1 :: forall t a b. AsyncTask t => (t a -> t b) -> AE t a -> AE t b
liftP1 f = AE <<< map f <<< runAE

liftP2 :: forall t a b c. AsyncTask t => (t a -> t b -> t c) -> AE t a -> AE t b -> AE t c
liftP2 f aeA aeB = AE $ lift2 f (runAE aeA) (runAE aeB)

instance functorAE :: AsyncTask t => Functor (AE t) where
  map = liftP1 <<< map

instance applyAE :: AsyncTask t => Apply (AE t) where
  apply = liftP2 apply

instance applicativeAE :: AsyncTask t => Applicative (AE t) where
  pure = liftP0 <<< pure

instance bindAE :: AsyncTask t => Bind (AE t) where
  bind ae f = AE $ do
    p <- runAE ae
    next p $ runAE <<< f

instance AsyncTask t => Monad (AE t)

instance altAE :: AsyncTask t => Alt (AE t) where
  alt = liftP2 alt

instance plusAE :: AsyncTask t => Plus (AE t) where
  empty = AE $ pure empty

instance alternativeAE :: AsyncTask t => Alternative (AE t)

instance monadEffectAE :: AsyncTask t => MonadEffect (AE t) where
  liftEffect eff = AE do
    a <- eff
    pure $ pure a

wait :: forall t a. AsyncTask t => t a -> AE t a
wait p = liftP0 p

fork :: forall t m a. AsyncTask t => MonadEffect m => AE t a -> m (t a)
fork = liftEffect <<< runAE
