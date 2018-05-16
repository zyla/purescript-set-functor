module Bench.Main where

import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)

import Bench.Data.Set.Functor (benchSetFunctor)

main :: Eff (console :: CONSOLE) Unit
main = do
  log "Set Functor"
  log "======"
  benchSetFunctor
