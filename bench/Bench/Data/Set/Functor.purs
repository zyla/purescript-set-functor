module Bench.Data.Set.Functor where

import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Performance.Minibench (bench)

import Data.List as L
import Data.Set as Set
import Data.Set.Functor as SetF

benchSetFunctor :: Eff (console :: CONSOLE) Unit
benchSetFunctor = do
  let
    nats = L.range 0 3000
    set = Set.fromFoldable nats
    setf = SetF.fromFoldable nats

    setMapped = Set.map id set
    setfMapped = map id setf

  log "Set.member"
  bench \_ -> Set.member 500 set

  log "SetF.member"
  bench \_ -> SetF.member 500 setf

  log "Set.member mapped"
  bench \_ -> Set.member 500 setMapped

  log "SetF.member mapped"
  bench \_ -> SetF.member 500 setfMapped
