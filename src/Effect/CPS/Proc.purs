module Effect.CPS.Proc
  ( Proc(..)
  , ProcCallback
  , class Fiber
  , fork
  , never
  , proc
  , runProc
  , launch
  , wait
  )
  where

import Prelude

import Control.Alternative (class Alt, class Alternative, class Plus)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import Effect (Effect)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Ref (modify_, new, read)

-- | Type alias for the CPS function with `Effect Unit` return type
type ProcCallback a = (a -> Effect Unit) -> Effect Unit

-- | A `Proc` is a CPS procedure. It's similar to `ContT Unit Effect`,
-- | but with a little enhancement in parallel `apply`, and race support
-- | via the `Alternative` type class.
newtype Proc a = Proc (ProcCallback a)

-- | Wrap a `ProcCallback` into a `Proc`
proc :: forall a. ProcCallback a -> Proc a
proc = Proc

-- | Unwrap a `Proc` to get the `ProcCallback`
runProc :: forall a. Proc a -> ProcCallback a
runProc (Proc prc) = prc

instance newtypeProc :: Newtype (Proc a) (ProcCallback a)

foreign import stackSafe :: forall a. (a -> Effect Unit) -> a -> Effect Unit

instance functorProc :: Functor Proc where
    -- | Map `Proc`s with the given function
    map f prcA = proc \r -> runProc prcA (stackSafe r <<< f)

instance applyProc :: Apply Proc where
    -- | Run `prcF` and `prcA` in parallel, return when both `Proc`s are
    -- | completed.
    apply prcF prcA = proc \r -> do
        refF <- new Nothing
        refA <- new Nothing
        runProc prcF \f -> read refA >>= case _ of
            Nothing -> modify_ (const $ Just f) refF
            Just a  -> stackSafe r (f a)
        runProc prcA \a -> read refF >>= case _ of
            Nothing -> modify_ (const $ Just a) refA
            Just f  -> stackSafe r (f a)

instance applicativeProc :: Applicative Proc where
    -- | Create a `Proc` that complete immediately with the given value.
    pure a = proc (_ $ a)

instance bindProc :: Bind Proc where
    -- | Chain up `Procs`.
    bind prcA f = proc \r -> runProc prcA
                       \a -> runProc (f a) (stackSafe r)

instance monadProc :: Monad Proc

instance monadEffectProc :: MonadEffect Proc where
    -- | Lift an `Effect` into a lazy-executed `Proc`
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
    -- | Race 2 `Proc`s
    alt prcX prcY = proc
        \r -> do
            r1 <- once $ stackSafe r
            runProc prcX r1
            runProc prcY r1

-- | A `Proc` that take forever.
never :: forall a. Proc a
never = proc <<< const <<< pure $ unit

instance plusProc :: Plus Proc where
    -- | `Proc` as an instance of `Plus`, `empty` is equivalent to `never`.
    empty = never

instance Alternative Proc

-- | A `Fiber` is a running `Proc`. It can be implemented with different
-- | concurrency model. An obviouse implementation is the `Proc` itself. It
-- | can be treated as a lazy synchronous execution of the `Proc`.
class Fiber f where
    -- | Kick off the `Fiber`.
    launch :: forall a. Proc a -> Effect (f a)
    -- | Wait the `Fiber` to complete execution and retrieve the value.
    wait :: forall a. f a -> Proc a

-- | Fork a `Proc` into a `Fiber`, this is the `launch` in `Proc` context.
fork :: forall f a. Fiber f => Proc a -> Proc (f a)
fork = liftEffect <<< launch

memoize :: forall a. Proc a -> Effect (Proc a)
memoize prc = do
        ref <- new Nothing
        pure $ proc
            \r -> read ref >>=
            case _ of
                Just a  -> stackSafe r a
                Nothing -> runProc prc \a -> do
                    modify_ (const $ Just a) ref
                    stackSafe r a

-- | A `Proc` itself is an instance of `Fiber`. It will be suspended on
-- | `launch`, and executed synchronously on `wait`
instance Fiber Proc where
    launch = memoize
    wait = identity
