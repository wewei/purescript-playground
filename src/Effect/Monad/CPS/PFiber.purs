module Effect.Monad.CPS.PFiber where

import Effect.Monad.CPS.Proc (class Fiber, Proc, TProc, launch, fork, proc, runProc)
import Prelude

import Effect (Effect)

foreign import data PFiber :: Type -> Type

foreign import ffiLaunchPFiber :: forall a. TProc a -> Effect (PFiber a)

foreign import ffiWaitPFiber :: forall a. PFiber a -> TProc a

instance fiberPFiber :: Fiber PFiber where
    launch = runProc >>> ffiLaunchPFiber
    wait = ffiWaitPFiber >>> proc

launchPFiber :: forall a. Proc a -> Effect (PFiber a)
launchPFiber = launch

launchPFiber_:: forall a. Proc a -> Effect Unit
launchPFiber_ = void <<< launchPFiber

forkPFiber :: forall a. Proc a -> Proc (PFiber a)
forkPFiber = fork

forkPFiber_ :: forall a. Proc a -> Proc Unit
forkPFiber_ = void <<< forkPFiber
