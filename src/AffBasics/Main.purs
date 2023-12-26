module AffBasics.Main where

import Prelude

import AffBasics.Delay as Delay
import AffBasics.LaunchAff as LaunchAff
import AffBasics.ForkJoin as ForkJoin
import AffBasics.SuspendJoin as SuspendJoin
import AffBasics.CachedJoin as CachedJoin
import AffBasics.SwitchingContexts as SwitchingContexts

import Effect (Effect)
import Effect.Console (log)

main :: Effect Unit
main = do
  log "[LaunchAff]"
  LaunchAff.main
  log "[Delay]"
  Delay.main
  log "[ForkJoin]"
  ForkJoin.main
  log "[SuspendJoin]"
  SuspendJoin.main
  log "[CachedJoin]"
  CachedJoin.main
  log "[SwitchingContexts]"
  SwitchingContexts.main

