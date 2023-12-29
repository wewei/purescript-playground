module Effect.CPS.PFiber
  ( PFiber
  , forkPFiber
  , forkPFiber_
  , launchPFiber
  , launchPFiber_
  )
  where

import Prelude

import Effect (Effect)
import Effect.CPS.Proc (class Fiber, Proc, ProcCallback, fork, launch, proc, runProc)

foreign import data PFiber :: Type -> Type

foreign import ffiLaunchPFiber :: forall a. ProcCallback a -> Effect (PFiber a)

foreign import ffiWaitPFiber :: forall a. PFiber a -> ProcCallback a

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
