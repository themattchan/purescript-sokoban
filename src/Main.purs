module Main where

import Prelude
import Effect (Effect)
import Effect.Console (log)


import Types

main :: forall e. Effect Unit
main = do
  log "Hello sailor!"
