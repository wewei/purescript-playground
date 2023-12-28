module Effect.CPS.Proc where

import Prelude

import Control.Alternative (class Alt, class Alternative, class Plus)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import Effect (Effect)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Ref (modify_, new, read)

type TProc a = (a -> Effect Unit) -> Effect Unit

newtype Proc a = Proc (TProc a)

proc :: forall a. TProc a -> Proc a
proc = Proc

runProc :: forall a. Proc a -> TProc a
runProc (Proc prc) = prc

instance newtypeProc :: Newtype (Proc a) (TProc a)

instance functorProc :: Functor Proc where
    map f prcA = proc \r -> runProc prcA (r <<< f)

instance applyProc :: Apply Proc where
    apply prcF prcA = proc \r -> do
        refF <- new Nothing
        refA <- new Nothing
        runProc prcF \f -> read refA >>= case _ of
            Nothing -> modify_ (const $ Just f) refF
            Just a  -> r (f a)
        runProc prcA \a -> read refF >>= case _ of
            Nothing -> modify_ (const $ Just a) refA
            Just f  -> r (f a)

instance applicativeProc :: Applicative Proc where
    pure a = proc (_ $ a)

instance bindProc :: Bind Proc where
    bind prcA f = proc \r -> runProc prcA
                       \a -> runProc (f a) r

instance monadProc :: Monad Proc

instance monadEffectProc :: MonadEffect Proc where
    liftEffect = proc <<< bind

once :: forall a. (a -> Effect Unit) -> Effect (a -> Effect Unit)
once f = do
    ref <- new true
    pure \a -> read ref >>=
         \c -> if c
                then do modify_ (const false) ref
                        f a
                else pure unit

instance altProc :: Alt Proc where
    alt prcX prcY = proc
        \r -> do
            r1 <- once r
            runProc prcX r1
            runProc prcY r1

never :: forall a. Proc a
never = proc <<< const <<< pure $ unit

instance plusProc :: Plus Proc where
    empty = never

instance Alternative Proc

class Fiber f where
    launch :: forall a. Proc a -> Effect (f a)
    wait :: forall a. f a -> Proc a

fork :: forall f a. Fiber f => Proc a -> Proc (f a)
fork = liftEffect <<< launch

memoize :: forall a. Proc a -> Effect (Proc a)
memoize prc = do
        ref <- new Nothing
        pure $ proc
            \r -> read ref >>=
            case _ of
                Just a  -> r a
                Nothing -> runProc prc \a -> do
                    modify_ (const $ Just a) ref
                    r a

instance Fiber Proc where
    launch = memoize
    wait = identity
