module Main where

import Prelude

import AffBasics.Main as AffBasics
import Effect (Effect)
import Effect.Console (log)
import AEExamples.Main as AEExamples

main :: Effect Unit
main = do
  when false do
    log "[AffBasics]"
    AffBasics.main
  log "[Effect]"
  AEExamples.main

