module Main where

import Prelude

import Control.Monad.Eff
import Control.Monad.Eff.Random (RANDOM())
import qualified Control.Monad.Eff.Console as Co


gen ::forall e. Array String -> Eff ( random :: RANDOM | e ) String
gen = Markov.genBars


main :: forall e. Eff ( console :: Co.CONSOLE | e ) Unit
main =
  Co.log "Young Markov, Cash Markov"
