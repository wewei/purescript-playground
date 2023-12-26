module Effect.AE where

import Prelude

import Control.Alt (class Alt)
import Control.Alternative (class Alternative)
import Control.Monad.Cont (ContT(..), runContT)
import Control.Plus (class Plus)
import Data.Maybe (Maybe(..))
import Data.Time.Duration (Milliseconds(..))
import Data.Tuple (Tuple, uncurry)
import Data.Tuple.Nested ((/\))
import Effect (Effect)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Ref (modify_, new, read)

foreign import data AE :: Type -> Type

foreign import makeAEImpl :: forall a. ((a -> Effect Unit) -> Effect Unit) -> AE a

makeAE :: forall a. ContT Unit Effect a -> AE a
makeAE = makeAEImpl <<< runContT

foreign import runAEImpl :: forall a. AE a -> (a -> Effect Unit) -> Effect Unit
runAE :: forall a. AE a -> ContT Unit Effect a
runAE = ContT <<< runAEImpl

instance functorAE :: Functor AE where
    map f = makeAE <<< map f <<< runAE

instance applyAE :: Apply AE where
    apply aeF aeA = map (uncurry ($)) $ both aeF aeA
    -- apply aeF aeA = makeAE $ (runAE aeF <*> runAE aeA)

instance applicative :: Applicative AE where
    pure = makeAE <<< pure

instance bind :: Bind AE where
    bind ae f = makeAE $ runAE ae >>= runAE <<< f

instance Monad AE

instance monadEffectAE :: MonadEffect AE where
    liftEffect = makeAE <<< liftEffect

instance altAE :: Alt AE where
    alt aeX aeY = makeAE <<< ContT $ \rt -> do
        (_ `runContT` rt) <<< runAE $ aeX
        (_ `runContT` rt) <<< runAE $ aeY

instance plusAE :: Plus AE where
  empty = makeAE <<< ContT <<< const <<< pure $ unit

instance alternative :: Alternative AE

foreign import delayImpl :: Number -> AE Unit

delay :: Milliseconds -> AE Unit
delay (Milliseconds ms) = delayImpl ms

launchAE_ :: forall a. AE a -> Effect Unit
launchAE_ = (_ `runContT` (void <<< pure)) <<< runAE



both :: forall a. forall b. AE a -> AE b -> AE (Tuple a b)
both aeA aeB = makeAE <<< ContT $ \rt -> do
    refA <- liftEffect $ new Nothing
    refB <- liftEffect $ new Nothing
    runContT (runAE aeA) $ \a -> read refB >>=
        case _ of
            Nothing -> modify_ (const <<< Just $ a) refA
            Just b  -> rt (a /\ b)
    runContT (runAE aeB) $ \b -> read refA >>=
        case _ of
            Nothing -> modify_ (const <<< Just $ b) refB
            Just a  -> rt (a /\ b)

infixr 6 both as <&>