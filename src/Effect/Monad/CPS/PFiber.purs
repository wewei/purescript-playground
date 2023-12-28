module Effect.Monad.CPS.PFiber where

import Effect.Monad.CPS.Proc (class Fiber, Proc, TProc, fork, launchProc, proc, runProc)
import Prelude

import Effect (Effect)

foreign import data PFiber :: Type -> Type

foreign import launchPFiber :: forall a. TProc a -> Effect (PFiber a)

foreign import waitPFiber :: forall a. PFiber a -> TProc a

instance fiberPFiber :: Fiber PFiber where
    launch = runProc >>> launchPFiber
    wait = waitPFiber >>> proc

launchPFiberProc :: forall a. Proc a -> Effect (PFiber a)
launchPFiberProc = launchProc

launchPFiberProc_ :: forall a. Proc a -> Effect Unit
launchPFiberProc_ = void <<< launchPFiberProc

forkPFiber :: forall a. Proc a -> Proc (PFiber a)
forkPFiber = fork

forkPFiber_ :: forall a. Proc a -> Proc Unit
forkPFiber_ = void <<< forkPFiber
